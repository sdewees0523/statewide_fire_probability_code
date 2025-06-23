library(here)
library(tidyverse)
library(stars, lib = "~/Rlibs")
library(mgcv)

# Loading in full model 
load(file = here("02_WRF", "models", "ffwi_full_model.rda"))

# Loading in aet raster 
aet_norm <- read_stars(here("00_Data_Assembly","data","climate_normals", "aet_Norm.tif"))

# Loading in shapefile of California and making sure it has same CRS as aet
ca <- st_read(here("00_Data_Assembly", "data", "ca_state", "CA_State.shp")) %>% 
  st_transform(crs = st_crs(aet_norm)) 

# Cropping aet to only have pixels inside California
aet_norm <- aet_norm %>% 
  st_crop(ca)

# Reading in other time stable rasters, snapping them to aet, and cropping to only have pixels inside California 
cultivated <- read_stars(here("00_Data_Assembly", "data", "cultivated", "Cultivated_Proportion_NullCorr.tif"))%>% 
  st_warp(aet_norm) %>% 
  st_crop(ca)
cwd_norm <- read_stars(here("00_Data_Assembly","data","climate_normals", "cwd_Norm.tif"))%>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)
road_dist <- read_stars(here("00_Data_Assembly","data","roads", "PrimSecRoads_Dist_km.tif"))%>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)
elec_dist <- read_stars(here("00_Data_Assembly","data","electric", "transmissionLines_Dist.tif"))%>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)
fveg <- read_stars(here("00_Data_Assembly","data", "fveg", "fveg.tif"))%>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)
mask <- read_stars(here("00_Data_Assembly","data", "buffer", "StatePoly_buf_water_50pct.tif")) %>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)

# Adding all time stable rasters to a raster list for easy referencing 
time_stable <- c(cultivated,
                 aet_norm,
                 cwd_norm,
                 road_dist,
                 elec_dist,
                 fveg,
                 mask) 

# Defining years of interest
years <- 2003:2016

# Setting aet raster as template
template <- aet_norm  

# Define base paths and file name patterns for time dynamic rasters
raster_sources <- list(
  housing    = "mean_housing/DenMean25km_%d.tif",
  aet_dev    = "aet/aet%d_dev.tif",
  cwd_dev    = "cwd/cwd%d_dev.tif",
  ffwi       = "fosberg/Fosberg_98_%d_gw.tif",
  time_fire  = "YearsSinceFire/YearsSinceFire_%d.tif",
  fire       = "fires/fire_%d_%d.tif"
)

# Create an empty list to hold all raster lists
all_rasters <- list()

# Loop over each raster type
for (var in names(raster_sources)) {
  message("Loading ", var, " rasters...") # Printing out message for progress tracking
  
  # Use map to read rasters for each year and warp
  all_rasters[[var]] <- map(years, function(y) {
    path <- here("00_Data_Assembly", "data", sprintf(raster_sources[[var]], y, y))
    r <- read_stars(path) %>% st_warp(template) %>% st_crop(ca)
    return(r)
  })
  # Labeling each raster in list by year it represents
  names(all_rasters[[var]]) <- as.character(years)
}

# Update each year's time fire raster to hold the statewide max value
fire_max_rasters <- all_rasters
fire_max_rasters$time_fire <- map(fire_max_rasters$time_fire, function(r) {
  max_val <- max(r[[1]], na.rm = TRUE)
  r[[1]][] <- max_val
  return(r)
})

# Making empty list for subsequent for loop
fire_raster_list <- list()

# Loop through each year
for (yr in years) {
  message("Processing year: ", yr) # Tracking progress
  
  # Build input data
  california_sf <- c( # Making list of all independent variable rasters
    time_stable,
    fire_max_rasters$housing[[as.character(yr)]],
    fire_max_rasters$aet_dev[[as.character(yr)]],
    fire_max_rasters$cwd_dev[[as.character(yr)]],
    fire_max_rasters$ffwi[[as.character(yr)]],
    fire_max_rasters$time_fire[[as.character(yr)]]
  ) %>%
    st_as_sf() %>% # Turning raster into point shapefile
    drop_na() %>% # Removing any rows with NA values
    rename( # Renaming columns to the variable names the model expects
      cultivated = Cultivated_Proportion_NullCorr.tif,
      housing = paste0("DenMean25km_", yr, ".tif"),
      aet_norm = aet_Norm.tif,
      cwd_norm = cwd_Norm.tif,
      aet_dev = paste0("aet", yr, "_dev.tif"),
      cwd_dev = paste0("cwd", yr, "_dev.tif"),
      road_dist = PrimSecRoads_Dist_km.tif,
      elec_dist = transmissionLines_Dist.tif,
      vegetation = fveg.tif,
      ffwi = paste0("Fosberg_98_", yr, "_gw.tif"),
      years_since_fire = paste0("YearsSinceFire_", yr, ".tif")
    ) %>%
    mutate( # Relabelling WHR10 codes to vegetation type
      vegetation = case_when(
        vegetation == 10 ~ "agriculture",
        vegetation == 20 ~ "barren/other",
        vegetation == 30 ~ "conifer",
        vegetation == 40 ~ "desert",
        vegetation == 50 ~ "hardwood",
        vegetation == 60 ~ "herbaceous",
        vegetation == 70 ~ "shrub",
        vegetation == 80 ~ "urban",
        vegetation == 90 ~ "water",
        vegetation == 100 ~ "wetland"
      )
    ) %>%
    filter(vegetation %in% c("conifer", "desert", "hardwood", "herbaceous", "shrub")) %>% # Filtering out vegetation the model wasn't trained on
    mutate(
      vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", TRUE ~ vegetation)) %>% # Combining both tree types into general tree category
    mutate(
      fire_prob = predict(ffwi_full_model, newdata = ., type = "response") # Predicting fire probability with years_since_fire set to statewide max
    ) %>%
    select(fire_prob, geometry) # Getting rid of all columns but predicted fire probability and geometry 
  
  # Rasterize fire_prob predictions
  fire_prob_raster <- st_rasterize(california_sf, template = aet_norm, align = TRUE)
  
  # Store result in list
  fire_raster_list[[as.character(yr)]] <- fire_prob_raster
}

# Combine into time cube and compute pixel-wise mean
# Turning list into raster stack
fire_raster_stack <- do.call(c, fire_raster_list)
# Naming each raster in stack by year it represents
names(fire_raster_stack) <- as.character(years)

# Making raster timecube from raster stack
fire_raster_timecube <- st_redimension(fire_raster_stack, along = list(time = as.character(years)))

# Calculating each pixels mean fire probability from the years represented in the timecube
mean_fire_prob <- st_apply(fire_raster_timecube, c("x", "y"), mean, na.rm = TRUE)

# Writing mean fire probability to disk
write_stars(mean_fire_prob, here("02_WRF", "Data", "fire_mean_predict_2003_2016_average.tif"), append = F)

# Loading in average predicted fire probability from actual statewide data
actual_fire_prob <- read_stars(here("02_WRF", "Data", "ffwi_full_model_predict_2003_2016_average.tif"))

# Calculating contribution of years since fire in to predicted fire probability 
fire_difference <- mean_fire_prob - actual_fire_prob

# Writing contribution raster to disk
write_stars(fire_difference, here("02_WRF", "Data", "fire_spatial_effect.tif"))


