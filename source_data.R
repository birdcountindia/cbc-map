library(googlesheets4)
library(tidyverse)
library(glue)
library(sf)
library(DBI)
library(geojsonsf)
library(rebird)
library(httr)
library(jsonlite)

update_map_data <- function() {

data<- read_sheet("https://docs.google.com/spreadsheets/d/1xNm-JheLupRkwpOTLcpit0RiTu6cmX-Y4We7S8vZAWk/", 
                       sheet = 1, col_names = TRUE)

# Process fresh data
data <- data %>%
 setNames(c("state", "city", "campus", "is_public", "date", "time", "lead_name", "email", "phone", "campus_type", "link")) %>%
  filter(!is.na(link)) |> 
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
  ) |> 
  ungroup() |> 
  filter(!is.na(hotspot_id))

load("data/data_old.RData") 

if (identical(data, data_old)) {
  message("--- No changes detected. Skipping update. ---")
  return(FALSE)
} else {
  message("--- Changes detected! Proceeding with update... ---")

source("data/private.R")
get_loc <- function(loc_id) {
  if (is.na(loc_id) || loc_id == "") return(tibble(latitude = NA, longitude = NA))
  url <- paste0("https://api.ebird.org/v2/ref/hotspot/info/", loc_id)
  res <- try(httr::GET(url, httr::add_headers("X-eBirdApiToken" = api_key)), silent = TRUE)
  if (!inherits(res, "try-error") && httr::status_code(res) == 200) {
    payload <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    return(tibble(
      latitude = as.numeric(payload$latitude),
      longitude = as.numeric(payload$longitude)
    ))
  }
  return(tibble(latitude = NA, longitude = NA))
}
 
 stats_df <- map_dfr(data$hotspot_id, ~ {
  loc_data <- get_loc(.x)
  Sys.sleep(0.2) 
  return(loc_data)
})

  data <- data %>%
    bind_cols(stats_df) %>%
    filter(!is.na(longitude)) |> 
    mutate(across(c(longitude, latitude), as.numeric)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)%>%
    mutate(across(where(is.character), ~ {
      clean_text <- str_replace_all(., "[^[:ascii:]]", " ") 
      str_squish(clean_text)
    })) %>%
    mutate(date_display = as.character(date))
  
  sf_geojson(data) %>% 
  write("campuses.json")
  
  data_old <- data
  save(data_old, file = "data/data_old.RData")
  
  message("--- Map data successfully updated. ---")
  return(TRUE)
}
}