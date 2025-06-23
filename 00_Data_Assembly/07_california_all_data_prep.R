library(here)
library(tidyverse)
library(stars, lib = "~/Rlibs")


# Load and prepare static layers.
## Using cultivated as template and making sure all other rasters are aligned. 
cultivated <- read_stars(here("data", "cultivated", "Cultivated_Proportion_NullCorr.tif"))
aet_norm <- read_stars(here("data","climate_normals", "aet_Norm.tif"))%>% 
  st_warp(cultivated)
cwd_norm <- read_stars(here("data","climate_normals", "cwd_Norm.tif"))%>% 
  st_warp(cultivated)
road_dist <- read_stars(here("data","roads", "PrimSecRoads_Dist_km.tif"))%>% 
  st_warp(cultivated)
elec_dist <- read_stars(here("data","electric", "transmissionLines_Dist.tif"))%>% 
  st_warp(cultivated)
fveg <- read_stars(here("data", "fveg", "fveg.tif"))%>% 
  st_warp(cultivated)
mask <- read_stars(here("data", "buffer", "StatePoly_buf_water_50pct.tif")) %>% 
  st_warp(cultivated)

## Combining all static layers into raster list for easy reference. 
time_stable <- c(cultivated,
                 aet_norm,
                 cwd_norm,
                 road_dist,
                 elec_dist,
                 fveg,
                 mask) 
# Reading in temporally dynamic raster layers. 

## Defining years of interest.
years <- 2003:2016

## Defining variables of interest. 
variables <- c("housing", "aet_dev", "cwd_dev", "ffwi", "time_fire", "fire")

## Defining file patterns.
file_patterns <- list(
  housing   = "mean_housing/DenMean25km_%d.tif",
  aet_dev   = "aet/aet%d_dev.tif",
  cwd_dev   = "cwd/cwd%d_dev.tif",
  ffwi      = "fosberg/Fosberg_98_%d_gw.tif",
  time_fire = "YearsSinceFire/YearsSinceFire_%d.tif",
  fire      = "fires/fire_%d_%d.tif"
)

## Read and warp year-varying files.
yearly_data <- map(years, function(yr) {
  map2(variables, file_patterns, function(var, pattern) {
    if (str_count(pattern, "%d") == 2) {
      filename <- sprintf(pattern, yr, yr)
    } else {
      filename <- sprintf(pattern, yr)
    }
    read_stars(here("data", filename)) %>% st_warp(cultivated)
  }) %>%
    set_names(variables)
}) %>%
  set_names(paste0("year_", years))

# Making empty list for subsequent for loop to occupy. 
california_list <- list()

# Looping through the years of interest to make shapefile for all of California.
for (years in 2003:2016) {
  year_label <- paste0("year_", years) # Making year label for reference.
  years_name <- as.character(years) # Making character value of year for later use.
  california <- c(time_stable, # Making raster list of all independent raster variables.
                  yearly_data[[year_label]]$housing,
                  yearly_data[[year_label]]$aet_dev,
                  yearly_data[[year_label]]$cwd_dev,
                  yearly_data[[year_label]]$ffwi,
                  yearly_data[[year_label]]$time_fire,
                  yearly_data[[year_label]]$fire) %>% 
    st_as_sf() %>% # Turning raster stack into point shapefile.
    rename(cultivated = Cultivated_Proportion_NullCorr.tif, # Renaming shapefile columns to easier to reference names. 
           housing = paste0("DenMean25km_", years_name, ".tif"),
           aet_norm = aet_Norm.tif,
           cwd_norm = cwd_Norm.tif,
           aet_dev = paste0("aet", years, "_dev.tif"),
           cwd_dev = paste0("cwd", years, "_dev.tif"),
           road_dist = PrimSecRoads_Dist_km.tif,
           elec_dist = transmissionLines_Dist.tif,
           vegetation = fveg.tif,
           ffwi = paste0("Fosberg_98_", years, "_gw.tif"),
           years_since_fire = paste0("YearsSinceFire_", years, ".tif"),
           fire = paste0("fire_", years, "_", years, ".tif"))%>% 
    mutate(fire = case_when(is.na(fire) == T ~ 0, # Setting fire column values to 0 for unburned and 1 for burned. This is the dependent variable. 
                            is.na(fire) == F ~ 1),
           vegetation = case_when(vegetation == 10 ~ "agriculture", # Relabelling WHR10 vegetation codes to vegetation type. 
                                  vegetation == 20 ~ "barren/other",
                                  vegetation == 30 ~ "conifer",
                                  vegetation == 40 ~ "desert",
                                  vegetation == 50 ~ "hardwood",
                                  vegetation == 60 ~ "herbaceous",
                                  vegetation == 70 ~ "shrub",
                                  vegetation == 80 ~ "urban",
                                  vegetation == 90 ~ "water",
                                  vegetation == 100 ~ "wetland")) %>%
    filter(StatePoly_buf_water_50pct.tif == 1) %>% # Filtering shapefile to exclude areas of water and where BCM data is not robust. 
    drop_na() %>% # Dropping any rows with NA values that the model won't be able to predict on. 
    mutate(year = years) %>% # Making column with the year we are working with. 
    st_as_sf() # Ensuring the file is still a shapefile after all the data cleaning. 
  
  california_list[[year_label]] <- california # Adding curent year's data to list created just before for loop. 
}

# Unlisting list of all years' data into one dataframe/shapefile. 
california_all_spatial <- do.call(rbind, california_list)

# Writing to disk. 
st_write(california_all_spatial, here("data", "Assembled_Data", "california_all.shp"), append = FALSE)

# Making non-spatial dataframe from above shapefile. 
california_all <- california_all_spatial %>%
  st_drop_geometry()

# Writing to disk. 
write.csv(california_all, here("data", "california_all.csv"), row.names = F, append = FALSE)

