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
  zoomControl = FALSE, 
  dragging = TRUE,
  tap = TRUE,          
  touchZoom = TRUE,
  worldCopyJump = FALSE
)) %>%
  setView(lng = 78.9629, lat = 22.5937, zoom = 5) %>%  
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}") %>%
 

# CSS --------
  prependContent(tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$style(HTML("
      body, html, #htmlwidget_container, .leaflet { 
        width: 100%; height: 100%; margin: 0; padding: 0; overflow: hidden; 
      }
      
      .close-x {
          position: absolute; top: 4px; right: 8px;
          font-size: 20px; color: #999; cursor: pointer;
          font-weight: normal; line-height: 1;
      }
      .close-x:hover { color: #000; }     

      .cbc-title { 
        font-weight: 900; text-align: center; text-transform: capitalize; 
        margin-bottom: 3px; font-size: 20px; color: #2c3e50; display: block;
      }
      .cbc-info-line { margin-bottom: 8px; font-size: 14px; text-align: center; }
      .cbc-info-line a {
        display: inline-block; padding: 10px 20px; background-color: #f8f9fa;
        border: 1px solid #007bff; border-radius: 8px; text-decoration: none;
        color: #007bff; font-weight: bold; margin-top: 3px;
      }

      #location-prompt {
        position: absolute; bottom: 15%; left: 50%;
        transform: translateX(-50%); z-index: 1000;
        background: white; padding: 10px 15px; border-radius: 10px;
        border: 1px solid #000; box-shadow: 0 4px 15px rgba(0,0,0,0.3); 
        font-size: 12px; font-family: Helvetica, sans-serif; font-weight: bold; 
        text-align: center; width: 80%; max-width: 350px;
      }
    
      #update-timer {
  position: absolute; 
  bottom: 12px;          /* same baseline as logo */
  left: 12px;
  z-index: 1000;
  background: rgba(255, 255, 255, 0.95); /* more solid white */
  padding: 8px 12px;
  border-radius: 8px;
  font-family: Helvetica, sans-serif; font-weight: bold;
  font-size: 11px;
  border: 1px solid #ddd;
  max-width: 180px;      /* consistent two-line wrap */
  text-align: center;
  line-height: 1.4;
  box-shadow: 0 4px 10px rgba(0,0,0,0.15); /* added shadow for depth */
  color: #333;
      }

  #bci-logo {
  position: absolute; 
  bottom: 12px;          /* same baseline as timer */
  right: 12px;
  z-index: 1000;
}

#bci-logo img {
  height: 65px;          /* slightly larger for visibility */
  width: auto;
  opacity: 1.0;          /* full visibility */
  border-radius: 12px;
  box-shadow: 0 4px 10px rgba(0,0,0,0.1); /* subtle shadow for logo */
}

/* Mobile adjustments to keep them from overlapping */
@media (max-width: 600px) {
  #update-timer { 
    font-size: 9px; 
    bottom: 10px; 
    left: 10px; 
    max-width: 140px; 
    padding: 6px;
  }
  #bci-logo img { 
    height: 48px; 
  }
  #bci-logo { 
    bottom: 10px; 
    right: 10px; 
  }
}
      
      @media only screen and (max-width: 768px) {
        .cbc-title { font-size: 18px; }
        .cbc-info-line { font-size: 12px; }
        .cbc-info-line a { font-size: 12px; padding: 10px 20px; }
      }
    "))
  )) %>%
  appendContent(tags$div(id = "location-prompt", 
                         tags$span(class="close-x", "×"),
                         "Granting location permission will allow you to view registered campuses nearby."),
                tags$div(id = "bci-logo", tags$img(src = "icons/bcilogo-framed.png")),
                tags$div(id = "update-timer", "Calculating last update...")) %>%
  
  onRender(paste0("
  function(el, x) {
    var map = this;
    var prompt = document.getElementById('location-prompt');
    
    if (prompt) {
        prompt.querySelector('.close-x').onclick = function() {
            prompt.style.display = 'none';
        };
    }

// --- 2. GITHUB TIMESTAMP TIMER LOGIC ---
    // Fetching from the raw GitHub URL
    var timestampUrl = 'https://raw.githubusercontent.com/birdcountindia/cbc-map/main/last_update.txt';
    
    fetch(timestampUrl)
      .then(response => response.text())
      .then(timestampStr => {
        const lastUpdate = new Date(timestampStr.trim());
        const timerElement = document.getElementById('update-timer');

        function updateCounter() {
          const now = new Date();
          const diffMs = now - lastUpdate;
          
          if (isNaN(diffMs)) {
            timerElement.innerHTML = 'Status: Online';
            return;
          }

          const diffHrs = Math.floor(diffMs / 3600000);
          const diffMins = Math.floor((diffMs % 3600000) / 60000);
          
          timerElement.innerHTML = \"Its been \" + diffHrs + \" hours and \" + diffMins + \" minutes since this map has been updated.\";
          
          // Visual warning if the automation has stalled (older than 2 hours)
          if (diffHrs >= 2) { 
            timerElement.style.color = '#e74c3c'; 
            timerElement.style.fontWeight = 'bold';
          } else {
            timerElement.style.color = '#555';
            timerElement.style.fontWeight = 'normal';
          }
        }

        updateCounter();
        setInterval(updateCounter, 60000); // Update every minute
      })
      .catch(err => {
        console.error('Timer fetch failed:', err);
        document.getElementById('update-timer').innerHTML = 'Status: Live';
      });
      
// --- Two-Finger Mobile / Standard PC Logic ---
    var size = 0.9; 
    var center = map.getCenter();
    var squareBounds = [[center.lat - size/2, center.lng - size/2], [center.lat + size/2, center.lng + size/2]];

    var redSquare = L.rectangle(squareBounds, {
      color: 'red', weight: 2, fillOpacity: 0.1, interactive: true
    }).addTo(map);

    var isDragging = false;
    var lastPos;

    function getEvtLatLng(e) {
      // For two-finger drag, we track the midpoint between the two fingers
      if (e.touches && e.touches.length >= 2) {
        var lat = (e.touches[0].pageY + e.touches[1].pageY) / 2;
        var lng = (e.touches[0].pageX + e.touches[1].pageX) / 2;
        return map.containerPointToLatLng([lng, lat]);
      }
      // For PC (Standard Mouse)
      return map.mouseEventToLatLng(e);
    }

    function onStart(e) {
      var nativeEvt = e.originalEvent || e;
      
      // PC: Trigger on mousedown
      // Mobile: Trigger ONLY if 2 fingers are touching
      if (nativeEvt.type === 'mousedown' || (nativeEvt.touches && nativeEvt.touches.length >= 2)) {
        isDragging = true;
        lastPos = getEvtLatLng(nativeEvt);
        
        map.dragging.disable();
        if (map.touchZoom) map.touchZoom.disable();
        
        L.DomEvent.stopPropagation(e);
      }
    }

    function onMove(e) {
      if (!isDragging) return;
      
      // Stop screen scrolling
      if (e.cancelable) e.preventDefault();

      var currentLatLng = getEvtLatLng(e);
      if (!currentLatLng || !lastPos) return;

      var deltaLat = currentLatLng.lat - lastPos.lat;
      var deltaLng = currentLatLng.lng - lastPos.lng;
      
      var b = redSquare.getBounds();
      redSquare.setBounds([
        [b.getSouth() + deltaLat, b.getWest() + deltaLng],
        [b.getNorth() + deltaLat, b.getEast() + deltaLng]
      ]);
      
      lastPos = currentLatLng;
    }

    function onEnd() {
      if (!isDragging) return;
      isDragging = false;
      map.dragging.enable();
      if (map.touchZoom) map.touchZoom.enable();
    }

    // A. Start dragging on the square
    redSquare.on('mousedown touchstart', onStart);

    // B. Track movement globally
    window.addEventListener('mousemove', onMove, {passive: false});
    window.addEventListener('touchmove', onMove, {passive: false});
    window.addEventListener('mouseup', onEnd);
    window.addEventListener('touchend', onEnd);

    // Load Data
    var dataUrl = 'https://raw.githubusercontent.com/birdcountindia/cbc-map/main/campuses.json';
    fetch(dataUrl)
      .then(response => response.json())
      .then(data => {
        L.geoJson(data, {
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
              \"<div class='cbc-info-line'><b>Participation:</b> \" + p.is_public + \"</div>\" +
              \"<div class='cbc-info-line'><b>Walks on:</b> \" + p.date_display + \"</div>\" +
              \"<div class='cbc-info-line'><b>Contact:</b> \" + p.lead_name + \" \" + (p.phone || '') + \"</div>\" +
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
          for (var i = 0; i < Math.min(3, dists.length); i++) bounds.extend(dists[i].m.getLatLng());
          var isMobile = window.innerWidth < 600;
          map.fitBounds(bounds, {
            padding: isMobile ? [30, 30] : [50, 50], 
            maxZoom: isMobile ? 14 : 12 
          });
          L.circleMarker(e.latlng, {radius: 8, fillColor: '#ff0000', color: '#fff', weight: 2}).addTo(map);
        });
      });
  }
  "))

saveWidget(map_shell, file = "CBC_map.html", selfcontained = TRUE)