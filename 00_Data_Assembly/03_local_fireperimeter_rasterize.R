library(here)
library(sf)
library(stars)
library(tidyverse)

## Reading in wildfire perimeters from Calfire Frap dataset
wildfires <- st_read(here("data", "fire_shapfiles", "wildfires.shp"))

## Making raster template from the bounding box of wildfires shapefile with 1km pixel resolution
raster_template <- st_as_stars(st_bbox(wildfires), dx = 1000, dy = 1000)

# Filtering wildfire polygons to just fires that occured in 2017
wildfires_2017 <- wildfires %>% 
  filter(YEAR_ == 2017) %>% 
  mutate(fire = 1) %>% 
  dplyr::select(fire, geometry) %>% 
  st_as_sf()

# Making 1km resolution raster of where fire did and did not occur in 2017
wildfires_2017_raster <- st_rasterize(wildfires_2017, raster_template, values = 1, fun = max, na.rm = TRUE)

## Repeating above for 2018-2023
wildfires_2018 <- wildfires %>% 
  filter(YEAR_ == 2018) %>% 
  mutate(fire = 1) %>% 
  dplyr::select(fire, geometry) %>% 
  st_as_sf()

wildfires_2018_raster <- st_rasterize(wildfires_2018, raster_template, values = 1, fun = max, na.rm = TRUE)

wildfires_2019 <- wildfires %>% 
  filter(YEAR_ == 2019) %>% 
  mutate(fire = 1) %>% 
  dplyr::select(fire, geometry) %>% 
  st_as_sf()

wildfires_2019_raster <- st_rasterize(wildfires_2019, raster_template, values = 1, fun = max, na.rm = TRUE)

wildfires_2020 <- wildfires %>% 
  filter(YEAR_ == 2020) %>% 
  mutate(fire = 1) %>% 
  dplyr::select(fire, geometry) %>% 
  st_as_sf()

wildfires_2020_raster <- st_rasterize(wildfires_2020, raster_template, values = 1, fun = max, na.rm = TRUE)

wildfires_2021 <- wildfires %>% 
  filter(YEAR_ == 2021) %>% 
  mutate(fire = 1) %>% 
  dplyr::select(fire, geometry) %>% 
  st_as_sf()

wildfires_2021_raster <- st_rasterize(wildfires_2021, raster_template, values = 1, fun = max, na.rm = TRUE)

wildfires_2022 <- wildfires %>% 
  filter(YEAR_ == 2022) %>% 
  mutate(fire = 1) %>% 
  dplyr::select(fire, geometry) %>% 
  st_as_sf()

wildfires_2022_raster <- st_rasterize(wildfires_2022, raster_template, values = 1, fun = max, na.rm = TRUE)

wildfires_2023 <- wildfires %>% 
  filter(YEAR_ == 2023) %>% 
  mutate(fire = 1) %>% 
  dplyr::select(fire, geometry) %>% 
  st_as_sf()

wildfires_2023_raster <- st_rasterize(wildfires_2023, raster_template, values = 1, fun = max, na.rm = TRUE)

## Writing fire rasters to disk
write_stars(wildfires_2020_raster, here("data", "fires", "fire_2020_2020.tif"))
write_stars(wildfires_2021_raster, here("data", "fires", "fire_2021_2021.tif"))
write_stars(wildfires_2022_raster, here("data", "fires", "fire_2022_2022.tif"))
write_stars(wildfires_2023_raster, here("data", "fires", "fire_2023_2023.tif"))


