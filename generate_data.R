library(sf)
library(geojsonsf)
library(stringr)
library(dplyr)
library(lubridate)

# 1. Load your raw data
load("cbc_app_data.RData") 

# 2. Clean 'unidentified' characters & Format Dates
# This fixes the "diamonds" and prepares the DD-Feb format
data_sf_cleaned <- data_sf %>%
  mutate(across(where(is.character), ~ {
    clean_text <- str_replace_all(., "[^\x00-\x7F]", " ") # Remove non-ASCII
    str_squish(clean_text)
  })) %>%
  mutate(
    date_obj = dmy(date),
    date_display = format(date_obj, "%d-%b")
  ) %>%
  select(-date_obj) # Remove helper column

# 3. Export as GeoJSON
# Upload this file to GitHub or GCP
if(!dir.exists("outputs")) dir.create("outputs")
sf_geojson(data_sf_cleaned) %>% 
  write("outputs/campuses.json")

cat("Success: campuses.json generated in /outputs/\n")