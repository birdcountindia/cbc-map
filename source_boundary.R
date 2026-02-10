library(sf)
library(rmapshaper) 
library(geojsonsf)
library(dplyr) 

load("../india-maps/outputs/maps_sf.RData") 

OUTPUT_FILE <- "in_boundary.json"
KEEP_PERCENT <- 0.01 

india_simple <- ms_simplify(india_sf, keep = KEEP_PERCENT, keep_shapes = TRUE)
india_simple <- st_transform(india_simple, 4326)

sf_geojson(india_simple) %>% write(OUTPUT_FILE)

rm(list = ls())
