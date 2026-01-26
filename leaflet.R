library(leaflet)
library(htmlwidgets)
library(htmltools)
library(sf)
library(base64enc)
library(lubridate)

# 1. Load Data
load("cbc_app_data.RData") 

# --- STEP A: ICON ENCODING (Base64) ---
# Only keeping the marker icon as requested
encode_svg <- function(path) {
  if (!file.exists(path)) stop(paste("Missing file at:", path))
  paste0("data:image/svg+xml;base64,", base64encode(path))
}

marker_uri <- encode_svg("www/icons/Map_marker.svg")

# 2. Build the Map
map_local <- leaflet() %>%
  # Clean Google Maps base layer
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}") %>%
  
  # Inject CSS and Centered Location Prompt
  prependContent(tags$head(
    includeCSS("www/style.css"),
    tags$style(HTML("
      #location-prompt {
        position: absolute;
        top: 50%; left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1000;
        background: white;
        padding: 15px 25px;
        border-radius: 20px;
        border: 2px solid #6f42c1;
        box-shadow: 0 4px 15px rgba(0,0,0,0.3);
        font-family: 'Trebuchet MS', sans-serif;
        font-size: 14px;
        font-weight: bold;
        text-align: center;
        width: 85%;
        max-width: 350px;
        pointer-events: none;
      }
    "))
  )) %>%
  appendContent(tags$div(id = "location-prompt", 
                         "Granting location permission will allow you to view registered campuses nearby.")) %>%
  
  # Campus Markers (No background polygons)
  addMarkers(
    data = data_sf,
    icon = makeIcon(iconUrl = marker_uri, iconWidth = 30, iconHeight = 30),
    group = "Campuses",
    popup = lapply(seq_len(nrow(data_sf)), function(i) {
      # Clean date format: DD-Feb
      clean_date <- format(dmy(data_sf$date[i]), "%d-%b")
      
      HTML(paste0(
        "<div class='cbc-hover-card'>",
        "<div class='cbc-title' style='text-transform: capitalize;'>", 
        data_sf$campus[i], " (", data_sf$city[i], ")", 
        "</div>",
        "<div class='cbc-info-line' style='margin-top: 10px;'>",
        "<a href='https://ebird.org/hotspot/", data_sf$hotspot_id[i], "' target='_blank' style='color:#0000FF; font-weight:bold; text-decoration:none;'><u>eBird Hotspot</u></a>",
        "</div>",
        "<div class='cbc-info-line'><b>Public walk:</b> ", data_sf$is_public[i], "</div>",
        "<div class='cbc-info-line'><b>Bird walks on:</b> ", clean_date, "</div>",
        "<div class='cbc-info-line'><b>Contact:</b> ", data_sf$lead_name[i], " (", data_sf$phone[i], ")</div>",
        "</div>"
      ))
    }),
    popupOptions = popupOptions(maxWidth = 320, minWidth = 260, closeOnClick = TRUE)
  ) %>%
  
  # Location and Bounding Box Logic
  onRender("
  function(el, x) {
    var map = this;
    var prompt = document.getElementById('location-prompt');

    map.locate({setView: false, enableHighAccuracy: true, timeout: 10000});

    map.on('locationfound', function(e) {
      if (prompt) { prompt.style.display = 'none'; }

      var allMarkers = [];
      map.eachLayer(function(layer) {
        if ((layer instanceof L.Marker || layer instanceof L.CircleMarker) && layer.options.group === 'Campuses') {
          allMarkers.push(layer);
        }
      });

      if (allMarkers.length === 0) return;

      var distances = allMarkers.map(function(marker) {
        return { marker: marker, distance: e.latlng.distanceTo(marker.getLatLng()) };
      });

      distances.sort(function(a, b) { return a.distance - b.distance; });

      var nearestBounds = L.latLngBounds().extend(e.latlng);
      var count = Math.min(5, distances.length);
      for (var i = 0; i < count; i++) {
        nearestBounds.extend(distances[i].marker.getLatLng());
      }

      map.fitBounds(nearestBounds, {padding: [50, 50], maxZoom: 12});

      L.circleMarker(e.latlng, {
        radius: 8, fillColor: '#ff0000', color: '#fff', weight: 2, group: 'user_loc'
      }).addTo(map);
    });

    map.on('locationerror', function(e) {
      if (prompt) {
        prompt.innerHTML = 'Location access denied. Please zoom manually.';
        prompt.style.border = '2px solid red';
        setTimeout(function(){ prompt.style.display = 'none'; }, 5000);
      }
    });
  }
")

# 3. Save as SINGLE PORTABLE FILE
if(!dir.exists("outputs")) dir.create("outputs")
saveWidget(map_local, file = "outputs/CBC_map.html", selfcontained = TRUE)