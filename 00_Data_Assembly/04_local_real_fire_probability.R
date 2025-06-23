library(here)
library(tidyverse)
library(stars)

## Set path to fires folder
fire_folder <- here("data", "fires")

## Make list of all fire tif files in fires folder
fire_files <- list.files(path = fire_folder, pattern = "\\.tif$", full.names = T)

## Read in all fire tifs to environment
fire_list <- lapply(fire_files, read_stars)

## Combine all fire tifs into a raster timecube
fire_stack <- do.call(c, c(fire_list, along = "time"))

## Get sum of fires that burned each pixel over 86 year timespan
fire_sum <- st_apply(fire_stack, MARGIN = c("x", "y"), FUN = sum, na.rm = T)

## Get yearly fire probability by dividing sum of fires by 86 years
fire_prob <- fire_sum/86

## Plotting to investigate data
ggplot()+
  geom_stars(data = fire_prob)
