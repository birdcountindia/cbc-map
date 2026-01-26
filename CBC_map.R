library(googlesheets4)
library(tidyverse)
library(glue)
library(sf)
library(DBI)
library(geojsonsf)

# --- 1. Load Data ---
source("../postgresql/private.R")
data0 <- read_sheet("https://docs.google.com/spreadsheets/d/1S0w0fDDGLfFd7Cwf5oUOyfivjBq8MphbR2OpT7I67pY/", 
                    sheet = 2,col_names = TRUE)
load("../india-maps/outputs/maps_sf.RData")

# --- 2. Clean Spatial Boundaries ---
sf_use_s2(FALSE)
states_sf <- states_sf %>%
  select(STATE.NAME) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 0.05) %>%
  st_make_valid()
sf_use_s2(TRUE)
rm(g1_in_sf, g2_in_sf, g3_in_sf, g4_in_sf, india_buff_sf, india_sf, grid_sizes_deg, grid_sizes_km)

# --- 3. Clean Campus Data ---
data <- data0 %>%
  filter(is.na(.[[12]]) | .[[12]] == "") %>%
  select(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) %>%
  rename(state = 1, city = 2, campus = 3, is_public = 4, date = 5, time = 6,
         lead_name = 7, email = 8, phone = 9, campus_type = 10, link = 11) %>%
  mutate(hotspot_id = str_extract(link, "L\\d+"),
         time = format(as.POSIXct(time), "%I:%M %p")) |> 
  group_by(hotspot_id) %>%
  mutate(
    date = dmy(date), 
    date = format(date, "%d-%b")
  ) %>%
  summarise(
    date = paste(unique(date), collapse = ", "),
    across(c(state, city, campus, is_public, time, lead_name, email, phone, campus_type, link), first),
    .groups = "drop"
  )
  

# --- 4. Fetch Stats from PostgreSQL ---
get_hotspot_stats <- function(loc_id, connection) {
  query <- glue("SELECT \"LOCALITY.ID\", MAX(\"LATITUDE\") AS latitude, MAX(\"LONGITUDE\") AS longitude,
                 COUNT(DISTINCT \"COMMON.NAME\") AS tot_sps,
                 COUNT(DISTINCT \"SAMPLING.EVENT.IDENTIFIER\") AS tot_lists,
                 COUNT(DISTINCT \"OBSERVER.ID\") AS tot_birders
                 FROM ebd WHERE \"ALL.SPECIES.REPORTED\" = TRUE AND \"LOCALITY.ID\" = '{loc_id}'
                 GROUP BY \"LOCALITY.ID\";")
  res <- dbGetQuery(connection, query)
  if(nrow(res) == 0) return(tibble(latitude = NA, longitude = NA, tot_sps = 0, tot_lists = 0, tot_birders = 0))
  return(as_tibble(res))
}

stats_df <- map_dfr(data$hotspot_id, ~get_hotspot_stats(.x, con))

# --- 5. Final SF Object ---
data_sf <- data %>%
  bind_cols(stats_df) %>%
  mutate(across(c(longitude, latitude), as.numeric)) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Save for App deployment
save(data_sf, states_sf, file = "cbc_app_data.RData")
rm(list = ls())
