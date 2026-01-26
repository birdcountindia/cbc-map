library(leaflet)
library(htmlwidgets)
library(htmltools)
library(sf)
library(base64enc)
library(lubridate)

# 1. Load your pre-processed data
load("cbc_app_data.RData") 

# 2. Define custom icon - using standard relative paths
cbc_icon <- makeIcon(
  iconUrl = "icons/Map_marker.svg",
  iconWidth = 35, iconHeight = 35,
  iconAnchorX = 17, iconAnchorY = 35
)

encode_svg <- function(path) {
  if (!file.exists(path)) stop(paste("Missing file at:", path))
  paste0("data:image/svg+xml;base64,", base64encode(path))
}

bird_uri   <- encode_svg("www/icons/bird.svg")
birder_uri <- encode_svg("www/icons/birder.svg")
list_uri   <- encode_svg("www/icons/list.svg")
marker_uri <- encode_svg("www/icons/Map_marker.svg")

# 3. Build the Map
map_local <- leaflet() %>%
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}") %>%
  
  prependContent(tags$head(
    includeCSS("www/style.css"),
    tags$style(HTML("
      #location-prompt {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1000;
        background: white;
        padding: 10px 20px;
        border-radius: 20px;
        border: 2px solid darkgrey;
        box-shadow: 0 4px 10px rgba(0,0,0,0.2);
        font-family: Trebuchet, sans-serif;
        font-size: 14px;
        font-weight: bold;
        text-align: center;
        width: 80%;
        max-width: 350px;
        pointer-events: none; /* Allows clicking the map through the box */
      }
    "))
  )) %>%
  appendContent(tags$div(id = "location-prompt", 
                         "Granting location permission will allow you to view registered campuses nearby.")) %>%
  
  addPolygons(data = states_sf, fillOpacity = 0, color = "black", weight = 1) %>%
  
  addMarkers(
    data = data_sf,
    icon = makeIcon(iconUrl = marker_uri, iconWidth = 30, iconHeight = 30),
    group = "Campuses",
    popup = lapply(seq_len(nrow(data_sf)), function(i) {
      HTML(paste0(
        "<div class='cbc-hover-card'>",
        "<div class='cbc-title'>", 
        data_sf$campus[i], ", ", data_sf$city[i],
        "</div>",
        "<div class='cbc-stats-row'>",
        # "<div class='cbc-stat-item'>",
        # "<div class='cbc-stat-value'>", as.integer(data_sf$tot_sps[i]), "</div>",
        # "<img src= bird.uri,"' class='cbc-stat-icon'>",
        # "</div>",
        # "<div class='cbc-stat-item'>",
        # "<div class='cbc-stat-value'>", as.integer(data_sf$tot_birders[i]), "</div>",
        # "<img src= birder.uri,"'  class='cbc-stat-icon'>",
        # "</div>",
        # "<div class='cbc-stat-item'>",
        # "<div class='cbc-stat-value'>", as.integer(data_sf$tot_lists[i]), "</div>",
        # "<img src= list.uri,"'  class='cbc-stat-icon'>",
        # "</div>",
        "</div>",
        "<div class='cbc-info-line'>",
        "<a href='https://ebird.org/hotspot/", data_sf$hotspot_id[i], "' target='_blank' style='color:#0000FF; font-weight:bold; text-decoration:none;'> <u>eBird Hotspot</u> </a>",
        "</div>",
        "<div class='cbc-info-line'><b>Public walk:</b> ", data_sf$is_public[i], "</div>",
        "<div class='cbc-info-line'><b>Bird walks on:</b> ", data_sf$date[i], "</div>",
        "<div class='cbc-info-line'><b>Contact:</b> ", data_sf$lead_name[i], " (", data_sf$phone[i], ")</div>",
        "</div>"
      ))
    }),
    popupOptions = popupOptions(maxWidth = 320, minWidth = 260, closeOnClick = TRUE)
  ) %>%
  onRender("
  function(el, x) {
    var map = this;
    var prompt = document.getElementById('location-prompt');

    // 1. Request location
      map.locate({
          setView: false, 
          enableHighAccuracy: true, // Uses GPS instead of just IP
          watch: false,             // Set to true only if you want to track movement
          timeout: 10000            // Give the mobile GPS 10 seconds to find a lock
      });
          map.on('locationfound', function(e) {
      console.log('Location found at:', e.latlng); // DEBUG
      if (prompt) { prompt.style.display = 'none'; }

      // 2. Identify the markers from the map layers
      // This is more reliable than searching the 'x' object
      var allMarkers = [];
      map.eachLayer(function(layer) {
        if (layer instanceof L.Marker || layer instanceof L.CircleMarker) {
          // Skip the user location marker we're about to add
          if (layer.options.group !== 'user_loc') {
            allMarkers.push(layer);
          }
        }
      });

      if (allMarkers.length === 0) {
        console.error('No markers found on map to calculate bounding box.');
        return;
      }

      // 3. Calculate distance to every marker
      var distances = allMarkers.map(function(marker, index) {
        return {
          marker: marker,
          distance: e.latlng.distanceTo(marker.getLatLng())
        };
      });

      // Sort by distance (ascending)
      distances.sort(function(a, b) { return a.distance - b.distance; });

      // 4. Create Bounding Box for nearest 5
      var nearestBounds = L.latLngBounds();
      nearestBounds.extend(e.latlng); // Include user
      
      var count = Math.min(5, distances.length);
      for (var i = 0; i < count; i++) {
        nearestBounds.extend(distances[i].marker.getLatLng());
      }

      // 5. Zoom to the nearest locations
      console.log('Zooming to bounds:', nearestBounds); // DEBUG
      map.fitBounds(nearestBounds, {padding: [50, 50], maxZoom: 12});

      // Add user location marker
      L.circleMarker(e.latlng, {
        radius: 8, fillColor: '#ff0000', color: '#fff', weight: 2, group: 'user_loc'
      }).addTo(map);
    });

    map.on('locationerror', function(e) {
      console.error('Location error:', e.message);
      if (prompt) {
        prompt.innerHTML = 'Location access denied. Please zoom manually.';
        prompt.style.border = '2px solid red';
        setTimeout(function(){ prompt.style.display = 'none'; }, 5000);
      }
    });
  }
")
  
# # 4. Attach Local Assets Dependency
# dep <- htmltools::htmlDependency(
#   name = "cbc-assets",
#   version = "1.0",
#   src = c(file = normalizePath("www")), # Ensure this folder contains icons/ and style.css
#   stylesheet = "style.css"
# )
# map_local$dependencies <- c(map_local$dependencies, list(dep))

saveWidget(map_local, 
           file = "outputs/CBC_map.html", 
           selfcontained = TRUE)
