library(leaflet)
library(htmlwidgets)
library(htmltools)
library(base64enc)

# --- STEP A: ICON ENCODING ---
encode_svg <- function(path) {
  if (!file.exists(path)) stop(paste("Missing file at:", path))
  paste0("data:image/svg+xml;base64,", base64encode(path))
}
marker_uri <- encode_svg("www/icons/Map_marker.svg")

# --- STEP B: BUILD SHELL ---
map_shell <- leaflet(options = leafletOptions(
  minZoom = 4,
  maxZoom = 18,
  zoomControl = FALSE, # Cleaner mobile UI
  dragging = TRUE,
  tap = TRUE,         
  touchZoom = TRUE,
  worldCopyJump = FALSE
)) %>%
  setView(lng = 78.9629, lat = 22.5937, zoom = 5) %>% 
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}") %>%
  
  prependContent(tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$style(HTML("
      body, html, #htmlwidget_container, .leaflet { 
        width: 100%; height: 100%; margin: 2; padding: 1; overflow: hidden; 
      }

      .cbc-title { 
        font-weight: 900; 
        text-align: center; 
        text-transform: capitalize; 
        margin-bottom: 3px; 
        font-size: 20px; 
        color: #2c3e50;
        display: block;
      }
      .cbc-info-line { 
        margin-bottom: 8px; 
        font-size: 14px; 
        text-align: center; 
      }
      .cbc-info-line a {
        display: inline-block;
        padding: 10px 20px;
        background-color: #f8f9fa;
        border: 1px solid #007bff;
        border-radius: 8px;
        text-decoration: none;
        color: #007bff;
        font-weight: bold;
        margin-top: 3px;
      }

      /* Mobile Location Prompt */
      #location-prompt {
        position: absolute; bottom: 15%; left: 50%;
        transform: translateX(-50%); z-index: 1000;
        background: white; padding: 6px 8px; border-radius: 10px;
        border: 1px solid #000; box-shadow: 0 4px 15px rgba(0,0,0,0.3); font-size: 12px;
        font-family: Helvetica, sans-serif; font-weight: bold; text-align: center;
        width: 80%; max-width: 350px;
      }

      /* Increase sizes for mobile screens */
      @media only screen and (max-width: 768px) {
        .cbc-title { font-size: 18px; }
        .cbc-info-line { font-size: 12px; }
        .cbc-info-line a { font-size: 12px; padding: 10px 20px; }
      }
    "))
  )) %>%
  appendContent(tags$div(id = "location-prompt", 
                         "Granting location permission will allow you to view registered campuses nearby.")) %>%
  
  onRender(paste0("
    function(el, x) {
      var map = this;
      var prompt = document.getElementById('location-prompt');
      var dataUrl = 'https://raw.githubusercontent.com/birdcountindia/cbc-map/main/campuses.json';

      fetch(dataUrl)
        .then(response => response.json())
        .then(data => {
          var campusLayer = L.geoJson(data, {
            pointToLayer: function (feature, latlng) {
              var isMobile = window.innerWidth < 600;
              var iconSize = isMobile ? [45, 45] : [35, 35];
              
              return L.marker(latlng, { 
                icon: L.icon({ 
                  iconUrl: '", marker_uri, "', 
                  iconSize: iconSize,
                  iconAnchor: [iconSize[0]/2, iconSize[1]]
                }),
                group: 'Campuses' 
              });
            },
            onEachFeature: function (feature, layer) {
              var p = feature.properties;
              layer.bindPopup(
                \"<div class='cbc-title'>\" + p.campus + \" (\" + p.city + \")</div>\" +
                \"<div class='cbc-info-line'><b>Public:</b> \" + p.is_public + \"</div>\" +
                \"<div class='cbc-info-line'><b>Walks on:</b> \" + p.date_display + \"</div>\" +
                \"<div class='cbc-info-line'><b>Contact:</b> \" + p.lead_name + \" (\" + p.phone + \")</div>\" +
                \"<div class='cbc-info-line'><a href='https://ebird.org/hotspot/\" + p.hotspot_id + \"' target='_blank'>eBird Hotspot</a></div>\"
              );
            }
          }).addTo(map);

          map.locate({setView: false, enableHighAccuracy: true, timeout: 10000});
          map.on('locationfound', function(e) {
            if (prompt) prompt.style.display = 'none';
            var allMarkers = [];
            map.eachLayer(function(l) { 
              if (l instanceof L.Marker && l.options.group === 'Campuses') allMarkers.push(l); 
            });
            var dists = allMarkers.map(m => ({ m: m, d: e.latlng.distanceTo(m.getLatLng()) }));
            dists.sort((a, b) => a.d - b.d);
            
            var bounds = L.latLngBounds().extend(e.latlng);
            for (var i = 0; i < Math.min(5, dists.length); i++) bounds.extend(dists[i].m.getLatLng());
            
            var isMobile = window.innerWidth < 600;
            map.fitBounds(bounds, {
              padding: isMobile ? [30, 30] : [50, 50], 
              maxZoom: isMobile ? 16 : 14 
            });
            L.circleMarker(e.latlng, {radius: 8, fillColor: '#ff0000', color: '#fff', weight: 2}).addTo(map);
          });
        });
    }
  "))

# 3. Save Tiny Portable Shell
saveWidget(map_shell, file = "CBC_map.html", selfcontained = TRUE)