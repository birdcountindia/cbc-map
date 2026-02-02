library(googlesheets4)
library(tidyverse)
library(glue)
library(sf)
library(DBI)
library(geojsonsf)

# --- 1. Load Data ---
source("../postgresql/private.R")
gs4_auth(email = "alenalex@ncf-india.org") # Ensure this is active for automation

# Pull fresh data
data_new <- read_sheet("https://docs.google.com/spreadsheets/d/1xNm-JheLupRkwpOTLcpit0RiTu6cmX-Y4We7S8vZAWk/", 
                       sheet = 1, col_names = TRUE)

# Process fresh data
data_processed <- data_new %>%
 setNames(c("state", "city", "campus", "is_public", "date", "time", "lead_name", "email", "phone", "campus_type", "link")) %>%
  mutate(hotspot_id = str_extract(link, "L\\d+"),
         time = format(as.POSIXct(time), "%I:%M %p")) %>% 
  group_by(hotspot_id) %>%
  mutate(
    date = dmy(date), 
    date = format(date, "%d-%b")
  ) %>%
  summarise(
    date = paste(unique(date), collapse = ", "),
    across(c(state, city, campus, is_public, time, lead_name, email, phone, campus_type, link), first),
    .groups = "drop"
  ) %>% 
  mutate(
    lead_name = if_else(is_public == "No", "Not ", lead_name),
    phone = if_else(is_public == "No", "Available", as.character(phone)),
    is_public = if_else(is_public == "No", "Closed to Public", "Open to Public")
  )

# --- 2. Comparison Check ---
if (file.exists("data/data_old.RData")) {
  load("data/data_old.RData") # This loads the 'data_old' object
} else {
  data_old <- NULL
}

if (!is.null(data_old) && identical(data_processed, data_old)) {
  
  message("--- No changes detected. Skipping update. ---")
  
} else {
  
  message("--- Changes detected! Proceeding with update... ---")
  
  # Update the saved reference
  data_old <- data_processed
  save(data_old, file = "data/data_old.RData")
  
  # --- 3. Core Processing (Geocoding & GeoJSON) ---
  get_loc <- function(loc_id, connection) {
    query <- glue("SELECT \"LOCALITY.ID\", 
                          MAX(\"LATITUDE\") AS latitude, 
                          MAX(\"LONGITUDE\") AS longitude
                   FROM ebd 
                   WHERE \"LOCALITY.ID\" = '{loc_id}'
                   GROUP BY \"LOCALITY.ID\";")
    res <- dbGetQuery(connection, query)
    if(nrow(res) == 0) return(tibble(latitude = NA, longitude = NA))
    return(as_tibble(res))
  }
  
  stats_df <- map_dfr(data_processed$hotspot_id, ~get_loc(.x, con))
  
  final_data <- data_processed %>%
    bind_cols(stats_df) %>%
    mutate(across(c(longitude, latitude), as.numeric)) %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  final_data <- final_data %>%
    mutate(across(where(is.character), ~ {
      clean_text <- str_replace_all(., "[^[:ascii:]]", " ") 
      str_squish(clean_text)
    })) %>%
    mutate(date_display = as.character(date))
  
  # Write the output
  sf_geojson(final_data) %>% 
    write("campuses.json")
  
  message("--- Map data successfully updated. ---")
}