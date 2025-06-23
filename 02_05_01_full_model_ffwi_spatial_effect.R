library(here)
library(tidyverse)
library(stars, lib = "~/Rlibs")
library(mgcv)

# Loading in full model trained on all years
load(file = here("02_WRF", "models", "ffwi_full_model.rda"))

# Loading in aet raster, also used as template raster
aet_norm <- read_stars(here("00_Data_Assembly","data","climate_normals", "aet_Norm.tif"))

# Reading in shapefile of California state boundary and ensuring CRS is same as aet raster
ca <- st_read(here("00_Data_Assembly", "data", "ca_state", "CA_State.shp")) %>% 
  st_transform(crs = st_crs(aet_norm)) 

# Cropping aet norm raster to only include pixels inside California
aet_norm <- aet_norm %>% 
  st_crop(ca)

# Reading in other time stable rasters, snapping them to aet template raster and cropping to only include pixels inside California
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

# Making raster list of time stable rasters for easy referencing
time_stable <- c(cultivated,
                 aet_norm,
                 cwd_norm,
                 road_dist,
                 elec_dist,
                 fveg,
                 mask) 
# Defining years of interest
years <- 2003:2016

# Setting aet raster as template raster
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

# Creating an empty list to hold all raster lists
all_rasters <- list()

# Loop over each time dynamic raster type
for (var in names(raster_sources)) {
  message("Loading ", var, " rasters...") # Making output message for each raster set for progress checking and referencing if code throws error
  
  # Using map to read rasters for each year and warp and keep in one dataframe
  all_rasters[[var]] <- map(years, function(y) {
    path <- here("00_Data_Assembly", "data", sprintf(raster_sources[[var]], y, y))
    r <- read_stars(path) %>% st_warp(template) %>% st_crop(ca)
    return(r)
  })
  
  names(all_rasters[[var]]) <- as.character(years) # labeling each raster in mapped dataframe by year if represents
}

# Updating each year's ffwi raster to hold the statewide mean value
ffwi_mean_rasters <- all_rasters
ffwi_mean_rasters$ffwi <- map(ffwi_mean_rasters$ffwi, function(r) {
  mean_val <- mean(r[[1]], na.rm = TRUE)
  r[[1]][] <- mean_val
  return(r)
})

# Making empty raster list for following for loop  
ffwi_raster_list <- list() 

# Loop through each year to make raster of predicted fire probabilities on new data with ffwi set to statewide mean for each pixel
for (yr in years) {
  message("Processing year: ", yr) # printing message for progress check and identifying where an error is thrown if occurs. 
  
  # Build input data
  california_sf <- c( # Making raster list of all independent variables 
    time_stable,
    ffwi_mean_rasters$housing[[as.character(yr)]],
    ffwi_mean_rasters$aet_dev[[as.character(yr)]],
    ffwi_mean_rasters$cwd_dev[[as.character(yr)]],
    ffwi_mean_rasters$ffwi[[as.character(yr)]],
    ffwi_mean_rasters$time_fire[[as.character(yr)]]
  ) %>%
    st_as_sf() %>% # Turning raster list into point shapefile
    drop_na() %>% # Removing any rows with na values
    rename( # renaming columns to the variable names the model expects
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
    mutate(
      vegetation = case_when( # Relabelling WHR10 codes to vegetation types
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
    filter(vegetation %in% c("conifer", "desert", "hardwood", "herbaceous", "shrub")) %>% # Filtering to only include vegetation model was trained on. 
    mutate(
      vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", TRUE ~ vegetation)) %>% # Combining two tree types to general tree category
    mutate(
      fire_prob = predict(ffwi_full_model, newdata = ., type = "response") # Making new column with predicted fire probability
    ) %>%
    select(fire_prob, geometry) # Getting rid of all columns but fire probability and geometry
  
  # Rasterize fire_prob predictions
  ffwi_prob_raster <- st_rasterize(california_sf, template = aet_norm, align = TRUE)
  
  # Store result
  ffwi_raster_list[[as.character(yr)]] <- ffwi_prob_raster
}

# Combine into time cube and compute pixel-wise mean
# Making raster stack from list created above
ffwi_raster_stack <- do.call(c, ffwi_raster_list) 
# Giving each raster in the list a name of the year it represents
names(ffwi_raster_stack) <- as.character(years)  

# Making raster timecube from above stack
ffwi_raster_timecube <- st_redimension(ffwi_raster_stack, along = list(time = as.character(years))) 

# Calculating each pixels mean value from the timecube
ffwi_mean_fire_prob <- st_apply(ffwi_raster_timecube, c("x", "y"), mean, na.rm = TRUE) 

# Writing raster of mean values to disk
write_stars(ffwi_mean_fire_prob, here("02_WRF", "Data", "ffwi_mean_predict_2003_2016_average.tif"), append = F)

# Reading in raster of mean values from actual statewide data
actual_fire_prob <- read_stars(here("02_WRF", "Data", "ffwi_full_model_predict_2003_2016_average.tif"))

# Calculating the ffwi contribution to fire probability
ffwi_difference <-  actual_fire_prob - ffwi_mean_fire_prob

# Writing ffwi contribution raster to disk
write_stars(ffwi_difference, here("02_WRF", "Data", "ffwi_spatial_effect.tif"))
