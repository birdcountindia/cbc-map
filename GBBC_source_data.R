library(googlesheets4)
library(tidyverse)
library(glue)
library(sf)
library(DBI)
library(geojsonsf)
library(rebird)
library(httr)
library(jsonlite)
library(tidygeocoder)

update_map_data_gbbc <- function() {
  
  data_gbbc<- read_sheet("https://docs.google.com/spreadsheets/d/1K-P3ebhqUs8JNpLI_6TjybCN6tMiiPuZVCikgif7jlw/", 
                    sheet = 1, col_names = TRUE)%>%
    select(1:3, 5:7, 14)
  
no_of_events_gbbc <- NROW(data_gbbc)
writeLines(as.character(no_of_events_gbbc), "no_of_events_gbbc.txt")
  
data_gbbc <- data_gbbc |>
  group_by(city, name) |>
  mutate(
    date = dmy(date),
    date = format(date, "%d-%b")
  ) |>
  summarise(
    date = paste(unique(date), collapse = ", "),
    across(c(state, email, phone, pin), first),
    .groups = "drop"
  ) |>
  mutate(
    pin = as.character(pin),
    geo_query = if_else(is.na(pin) | pin == "", city, pin)
  ) |>
  geocode(
    address = geo_query,
    method = "osm",
    lat = latitude,
    long = longitude
  )

  
    mutate(
      lead_name = if_else(is_public == "No", "Not ", lead_name),
      phone = if_else(is_public == "No", "Available", as.character(phone)),
      is_public = if_else(is_public == "No", "Closed to Public", "Open to Public")
    ) |> 
    ungroup() |> 
    filter(!is.na(hotspot_id))
  
  no_of_campuses <- NROW(data)
  writeLines(as.character(no_of_campuses), "no_of_campuses.txt")
  
  no_of_states <- unique(data$state) 
  no_of_states <-  n_distinct(no_of_states)
  writeLines(as.character(no_of_states), "no_of_states.txt")
  
  load("data/data_old.RData") 
  
  if (identical(data, data_old)) {
    message("--- No changes detected. Skipping update. ---")
    return(FALSE)
  } else {
    data_old <- data
    save(data_old, file = "data/data_old.RData")
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
    
    message("--- Map data successfully updated. ---")
    return(TRUE)
  }
}
