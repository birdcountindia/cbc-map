library(leaflet)
library(htmlwidgets)
library(htmltools)
library(base64enc)
library(geojsonsf)
library(sf)

# --- STEP 1: LOAD & PREPARE DATA ---
if (!file.exists("in_boundary.json")) stop("Error: 'in_boundary.json' missing.")

# 1. Load Boundary
in_simple <- read_sf("in_boundary.json")
india_json_str <- sf_geojson(in_simple)

# 2. Load Icon
encode_svg <- function(path) {
  if (!file.exists(path)) stop(paste("Missing file at:", path))
  paste0("data:image/svg+xml;base64,", base64encode(path))
}
marker_uri <- encode_svg("www/icons/Map_marker.svg")

# 3. Bundle Data
map_data_bundle <- list(
  boundary = as.character(india_json_str),
  marker = marker_uri
)

# --- STEP 2: JAVASCRIPT LOGIC ---
js_logic <- "
  function(el, x, bundledData) {
    var map = this;
    var markerIcon = bundledData.marker;
    var boundaryGeoJSON = JSON.parse(bundledData.boundary);

    // --- HELPER: Point in Polygon Algorithm ---
    function isInside(lat, lng) {
        var polys = [];
        if (boundaryGeoJSON.type === 'FeatureCollection') {
             boundaryGeoJSON.features.forEach(f => {
                 if(f.geometry.type === 'MultiPolygon') f.geometry.coordinates.forEach(p => polys.push(p));
                 else if (f.geometry.type === 'Polygon') polys.push(f.geometry.coordinates);
             });
        } else if (boundaryGeoJSON.type === 'Feature') {
             if(boundaryGeoJSON.geometry.type === 'MultiPolygon') polys = boundaryGeoJSON.geometry.coordinates;
             else polys = [boundaryGeoJSON.geometry.coordinates];
        }

        var x = lng, y = lat;
        for (var i = 0; i < polys.length; i++) {
            var ring = polys[i][0]; 
            var inPoly = false;
            for (var k = 0, j = ring.length - 1; k < ring.length; j = k++) {
                var xi = ring[k][0], yi = ring[k][1];
                var xj = ring[j][0], yj = ring[j][1];
                var intersect = ((yi > y) != (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi) + xi);
                if (intersect) inPoly = !inPoly;
            }
            if (inPoly) return true;
        }
        return false;
    }

    // 1. ADD CONTROLS
    L.control.zoom({ position: 'topright' }).addTo(map);
    
    var recenter = L.control({position: 'topright'});
    recenter.onAdd = function(map) {
      var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
      div.innerHTML = '<a href=\"#\" title=\"Reset View\" style=\"background-color: white; width: 30px; height: 30px; line-height: 30px; text-align: center; display: block; cursor: pointer; color: black; font-size: 22px; font-weight: bold; text-decoration: none;\">&#8962;</a>';
      div.onclick = function(e) {
        L.DomEvent.stopPropagation(e);
        L.DomEvent.preventDefault(e);
        map.setView([22.5937, 78.9629], 5);
      };
      return div;
    };
    recenter.addTo(map);

    var dashboard = L.control({position: 'topleft'});
    dashboard.onAdd = function (map) {
      var div = L.DomUtil.create('div', 'stats-dashboard');
      div.innerHTML = 
        '<div class=\"stat-item\"><div class=\"stat-value\" id=\"dash-events\">--</div><div class=\"stat-label\">Events</div></div>' +
        '<div class=\"stat-item\"><div class=\"stat-value\" id=\"dash-campuses\">--</div><div class=\"stat-label\">Campuses</div></div>' +
        '<div class=\"stat-item\"><div class=\"stat-value\" id=\"dash-states\">-- / 37</div><div class=\"stat-label\">States / UTs</div></div>';
      return div;
    };
    dashboard.addTo(map);

    // 2. FETCH DATA
    var baseUrl = 'https://raw.githubusercontent.com/birdcountindia/cbc-map/main/';
    fetch(baseUrl + 'no_of_events.txt').then(r => r.text()).then(t => { document.getElementById('dash-events').innerText = t.trim(); });
    fetch(baseUrl + 'no_of_campuses.txt').then(r => r.text()).then(t => { document.getElementById('dash-campuses').innerText = t.trim(); });
    fetch(baseUrl + 'no_of_states.txt').then(r => r.text()).then(t => { document.getElementById('dash-states').innerText = t.trim() + ' / 37'; });

    // --- TIMER LOGIC (UPDATED WITH YOUR CODE) ---
    fetch(baseUrl + 'last_update.txt').then(r => r.text()).then(ts => {
        var lastUpdate = new Date(ts.trim());
        var timerElement = document.getElementById('update-timer');
        
        function updateCounter() {
          var now = new Date();
          var diffMs = now - lastUpdate;
          
          if (isNaN(diffMs)) { 
             if(timerElement) timerElement.innerHTML = 'Status: Online'; 
             return; 
          }
          
          var diffHrs = Math.floor(diffMs / 3600000);
          var diffMins = Math.floor((diffMs % 3600000) / 60000);

          if (timerElement) {
              if (diffHrs > 0) {
                 timerElement.innerHTML = \"This map was last updated \" + diffHrs + \" hours and \" + diffMins + \" minutes ago.\";
              } else {
                 timerElement.innerHTML = \"This map was last updated \" + diffMins + \" minutes ago.\";
              }
              
              if (diffHrs >= 2) { 
                timerElement.style.color = '#e74c3c'; 
                timerElement.style.fontWeight = 'bold';
              } else {
                timerElement.style.color = '#555'; 
                timerElement.style.fontWeight = 'normal';
              }
          }
        }
        updateCounter(); 
        setInterval(updateCounter, 60000);
    });

    // 3. SQUARE LOGIC (Strict Mobile Check)
    fetch(baseUrl + 'square_size.txt').then(r => r.text()).then(sizeStr => {
        var isMobile = /Mobi|Android|iPhone|iPad|iPod/i.test(navigator.userAgent) || (window.innerWidth <= 768);
        
        if (!isMobile) {
            var size = parseFloat(sizeStr.trim()) || 2.25;
            var center = map.getCenter();
            var squareBounds = [[center.lat - size/2, center.lng - size/2], [center.lat + size/2, center.lng + size/2]];

            var redSquare = L.rectangle(squareBounds, { 
                color: 'red', 
                weight: 2, 
                fillOpacity: 0.1, 
                interactive: true,
                className: 'draggable-square' 
            }).addTo(map);

            var isDragging = false;
            var lastPos;

            function onStart(e) {
              isDragging = true;
              lastPos = e.latlng;
              map.dragging.disable();
              L.DomEvent.stopPropagation(e);
            }

            function onMove(e) {
              if (!isDragging || !redSquare) return;
              var currentLatLng = map.mouseEventToLatLng(e);
              if (isInside(currentLatLng.lat, currentLatLng.lng)) {
                  var deltaLat = currentLatLng.lat - lastPos.lat;
                  var deltaLng = currentLatLng.lng - lastPos.lng;
                  var b = redSquare.getBounds();
                  redSquare.setBounds([
                    [b.getSouth() + deltaLat, b.getWest() + deltaLng],
                    [b.getNorth() + deltaLat, b.getEast() + deltaLng]
                  ]);
                  lastPos = currentLatLng;
              }
            }

            function onEnd() { isDragging = false; map.dragging.enable(); }
            redSquare.on('mousedown', onStart);
            window.addEventListener('mousemove', onMove);
            window.addEventListener('mouseup', onEnd);
        }
    });

    // 4. CAMPUS MARKERS
    fetch(baseUrl + 'campuses.json').then(r => r.json()).then(data => {
        L.geoJson(data, {
          pointToLayer: function (feature, latlng) {
            var isMobile = window.innerWidth < 600;
            var iconSize = isMobile ? [35, 35] : [25, 25];
            return L.marker(latlng, { 
              icon: L.icon({ 
                iconUrl: markerIcon, 
                iconSize: iconSize, 
                iconAnchor: [iconSize[0]/2, iconSize[1]] 
              }),
              group: 'Campuses' 
            });
          },
          onEachFeature: function (f, l) {
            var p = f.properties;
            l.bindPopup(\"<div class='cbc-title'>\"+p.campus+\" (\"+p.city+\")</div><div class='cbc-info-line'><b>Participation:</b> \"+p.is_public+\"</div><div class='cbc-info-line'><b>Walks:</b> \"+p.date_display+\"</div><div class='cbc-info-line'><b>Contact:</b> \"+p.lead_name+\" \"+(p.phone||'')+\"</div><div class='cbc-info-line'><a href='https://ebird.org/hotspot/\"+p.hotspot_id+\"' target='_blank'>eBird Hotspot</a></div>\");
          }
        }).addTo(map);

        var prompt = document.getElementById('location-prompt');
        if (prompt) prompt.querySelector('.close-x').onclick = function(){ prompt.style.display='none'; };
        
        map.locate({setView: false, enableHighAccuracy: true, timeout: 10000});
        map.on('locationfound', function(e) {
          if (prompt) prompt.style.display = 'none';
          var allMarkers = [];
          map.eachLayer(function(l) { if (l instanceof L.Marker && l.options.group === 'Campuses') allMarkers.push(l); });
          var dists = allMarkers.map(m => ({ m: m, d: e.latlng.distanceTo(m.getLatLng()) })).sort((a, b) => a.d - b.d);
          var bounds = L.latLngBounds().extend(e.latlng);
          for (var i = 0; i < Math.min(5, dists.length); i++) bounds.extend(dists[i].m.getLatLng());
          map.fitBounds(bounds, { padding: [50, 50], maxZoom: 13 });
          L.circleMarker(e.latlng, {radius: 8, fillColor: '#f00', color: '#fff', weight: 2}).addTo(map);
        });
    });
  }
"

# --- STEP 3: BUILD MAP ---
map_shell <- leaflet(options = leafletOptions(
  minZoom = 4, maxZoom = 15, zoomControl = FALSE, dragging = TRUE, tap = TRUE, touchZoom = TRUE
)) %>%
  setView(lng = 78.9629, lat = 22.5937, zoom = 5) %>%
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}&apistyle=s.t:2|s.e:g|p.v:off") %>%
  addPolygons(data = in_simple, fill = FALSE, color = "#333333", weight = 1.5, opacity = 1.0, options = pathOptions(interactive = FALSE)) %>%
  prependContent(tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$style(HTML("
      body, html, #htmlwidget_container, .leaflet { width: 100%; height: 100%; margin: 0; padding: 0; overflow: hidden; }
      .close-x { position: absolute; top: 4px; right: 8px; font-size: 20px; color: #999; cursor: pointer; line-height: 1; }
      .cbc-title { font-weight: 900; text-align: center; text-transform: capitalize; margin-bottom: 3px; font-size: 20px; color: #2c3e50; }
      .cbc-info-line { margin-bottom: 8px; font-size: 14px; text-align: center; }
      .cbc-info-line a { display: inline-block; padding: 10px 20px; background-color: #f8f9fa; border: 1px solid #007bff; border-radius: 8px; text-decoration: none; color: #007bff; font-weight: bold; margin-top: 3px; }
      #location-prompt { position: absolute; bottom: 15%; left: 50%; transform: translateX(-50%); z-index: 1000; background: white; padding: 10px 15px; border-radius: 10px; border: 1px solid #000; box-shadow: 0 4px 15px rgba(0,0,0,0.3); font-size: 12px; font-family: Helvetica, sans-serif; font-weight: bold; text-align: center; width: 80%; max-width: 350px; }
      #update-timer { position: absolute; bottom: 12px; left: 12px; z-index: 1000; background: rgba(255, 255, 255, 0.95); padding: 8px 12px; border-radius: 8px; font-family: Helvetica, sans-serif; font-weight: bold; font-size: 11px; border: 1px solid #ddd; max-width: 180px; text-align: center; line-height: 1.4; box-shadow: 0 4px 10px rgba(0,0,0,0.15); color: #333; }
      #bci-logo { position: absolute; bottom: 12px; right: 12px; z-index: 1000; }
      #bci-logo img { height: 65px; width: auto; opacity: 1.0; border-radius: 12px; box-shadow: 0 4px 10px rgba(0,0,0,0.1); }
      .stats-dashboard { background: rgba(255, 255, 255, 0.95); padding: 8px 12px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.2); font-family: 'Helvetica Neue', Arial, sans-serif; min-width: 110px; margin-top: 10px !important; }
      .stat-item { margin-bottom: 8px; text-align: center; border-bottom: 1px solid #eee; padding-bottom: 4px; }
      .stat-item:last-child { border-bottom: none; margin-bottom: 0; }
      .stat-label { font-size: 10px; text-transform: uppercase; letter-spacing: 0.5px; color: #666; margin-top: -2px; }
      .stat-value { font-size: 26px; font-weight: 800; color: #e74c3c; line-height: 1.0; }
      @media (max-width: 600px) { #update-timer { font-size: 9px; bottom: 10px; left: 10px; max-width: 140px; padding: 6px; } #bci-logo img { height: 48px; } #bci-logo { bottom: 10px; right: 10px; } .stats-dashboard { transform: scale(0.8); transform-origin: top left; } }
      
      /* THE HAMMER: Force hide the square on mobile via CSS */
      @media (max-width: 768px) {
        path.draggable-square { display: none !important; }
      }
    "))
  )) %>%
  appendContent(tags$div(id = "location-prompt", tags$span(class="close-x", "×"), "Granting location permission will allow you to view registered campuses nearby."),
                tags$div(id = "bci-logo", tags$img(src = "icons/bcilogo-framed.png")),
                tags$div(id = "update-timer", "Calculating last update...")) %>%
  onRender(js_logic, data = map_data_bundle)

# --- STEP 4: SAVE ---
saveWidget(map_shell, file = "CBC_map.html", selfcontained = TRUE)