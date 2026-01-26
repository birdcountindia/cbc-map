library(googlesheets4)
library(tidyverse)
library(glue)
library(sf)
library(DBI)
library(geojsonsf)

# --- 1. Load Data ---
source("../postgresql/private.R")
data <- read_sheet("https://docs.google.com/spreadsheets/d/1S0w0fDDGLfFd7Cwf5oUOyfivjBq8MphbR2OpT7I67pY/", 
                    sheet = 2,col_names = TRUE)

data <- data %>%
  select(-12) %>%
  setNames(c("state", "city", "campus", "is_public", "date", "time", "lead_name", "email", "phone", "campus_type", "link")) %>%
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
  ) |> 
  mutate(
    lead_name = if_else(is_public == "No", "Not ", lead_name),
    phone = if_else(is_public == "No", "Available", as.character(phone)),
    is_public = if_else(is_public == "No", "Closed to Public", "Open to Public")
  )
  
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

stats_df <- map_dfr(data$hotspot_id, ~get_loc(.x, con))

data <- data %>%
  bind_cols(stats_df) %>%
  mutate(across(c(longitude, latitude), as.numeric)) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

data <- data %>%
  mutate(across(where(is.character), ~ {
    clean_text <- str_replace_all(., "[^[:ascii:]]", " ") 
    str_squish(clean_text)
  })) %>%
  mutate(
    date_display = as.character(date)
  )

sf_geojson(data) %>% 
  write("campuses.json")