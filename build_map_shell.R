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
map_shell <- leaflet() %>%
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}") %>%
  
  # Inject CSS and Centered Prompt
  prependContent(tags$head(
    tags$style(HTML("
      #location-prompt {
        position: absolute; top: 50%; left: 50%;
        transform: translate(-50%, -50%); z-index: 1000;
        background: white; padding: 20px 30px; border-radius: 20px;
        border: 3px solid #6f42c1; box-shadow: 0 4px 15px rgba(0,0,0,0.3);
        font-family: sans-serif; font-weight: bold; text-align: center;
        width: 85%; max-width: 400px;
      }
      .cbc-title { font-weight: 900; text-transform: capitalize; margin-bottom: 8px; }
    "))
  )) %>%
  appendContent(tags$div(id = "location-prompt", 
                         "Granting location permission will allow you to view registered campuses nearby.")) %>%
  
  onRender(paste0("
    function(el, x) {
      var map = this;
      var prompt = document.getElementById('location-prompt');
      
      // 1. SET YOUR HOSTED URL HERE
      var dataUrl = 'https://raw.githubusercontent.com/username/repo/main/campuses.json';

      // 2. Fetch the dynamic data
      fetch(dataUrl)
        .then(response => response.json())
        .then(data => {
          var campusLayer = L.geoJson(data, {
            pointToLayer: function (feature, latlng) {
              return L.marker(latlng, { 
                icon: L.icon({ iconUrl: '", marker_uri, "', iconSize: [30, 30] }),
                group: 'Campuses' 
              });
            },
            onEachFeature: function (feature, layer) {
              var p = feature.properties;
              layer.bindPopup(
                \"<div class='cbc-title'>\" + p.campus + \" (\" + p.city + \")</div>\" +
                \"<div class='cbc-info-line'><b>Public:</b> \" + p.is_public + \"</div>\" +
                \"<div class='cbc-info-line'><b>Walk on:</b> \" + p.date_display + \"</div>\" +
                \"<div class='cbc-info-line'><b>Contact:</b> \" + p.lead_name + \"</div>\"
              );
            }
          }).addTo(map);

          // 3. Location & Bounding Box Logic
          map.locate({setView: false, enableHighAccuracy: true});
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
            map.fitBounds(bounds, {padding: [50, 50], maxZoom: 12});
            L.circleMarker(e.latlng, {radius: 8, fillColor: '#ff0000', color: '#fff', weight: 2}).addTo(map);
          });
        });
    }
  "))

# 3. Save Tiny Portable Shell
saveWidget(map_shell, file = "outputs/CBC_map_shell.html", selfcontained = TRUE)