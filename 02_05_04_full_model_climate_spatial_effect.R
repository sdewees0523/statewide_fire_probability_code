library(here)
library(tidyverse)
library(stars, lib = "~/Rlibs")
library(mgcv)

# Loading in model trained on all years of training data
load(file = here("02_WRF", "models", "ffwi_full_model.rda"))

# Loading in aet norm raster
aet_norm <- read_stars(here("00_Data_Assembly","data","climate_normals", "aet_Norm.tif"))
# Loading in shapefile of california and making sure it is in same crs as aet norm raster
ca <- st_read(here("00_Data_Assembly", "data", "ca_state", "CA_State.shp")) %>% 
  st_transform(crs = st_crs(aet_norm)) 

# Cropping aet norm to only include pixels inside california
aet_norm <- aet_norm %>% 
  st_crop(ca)
# Calculating the state wide mean of aet_norm
aet_norm_mean <- mean(aet_norm[[1]], na.rm = T)
# Setting all pixel values to statewide mean
aet_norm[[1]]<- aet_norm_mean

# Reading in cwd norm, snapping to aet norm, cropping to only include pixels inside california
cwd_norm <- read_stars(here("00_Data_Assembly","data","climate_normals", "cwd_Norm.tif"))%>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)
# Calculating statewide mean for cwd norm
cwd_norm_mean <- mean(cwd_norm[[1]], na.rm = T)
# Setting all pixel values equal to state wide mean 
cwd_norm[[1]] <- cwd_norm_mean

# Reading in aet dev, snapping to aet norm, and cropping to only include pixels inside california
aet_dev <- read_stars(here("00_Data_Assembly", "data", "aet", "aet2003_dev.tif")) %>% 
  st_warp(aet_norm) %>% 
  st_crop(ca)
# Setting all pixel values equal to 0
aet_dev[[1]] <- 0

# Reading in cwd dev, snapping to aet norm, and cropping to only include pixels inside california
cwd_dev <- read_stars(here("00_Data_Assembly", "data", "cwd", "cwd2003_dev.tif")) %>% 
  st_warp(aet_norm) %>% 
  st_crop(ca)
# Setting all pixel values equal to 0
cwd_dev[[1]] <- 0

# Reading in the rest of the time stable variables
cultivated <- read_stars(here("00_Data_Assembly", "data", "cultivated", "Cultivated_Proportion_NullCorr.tif"))%>% 
  st_warp(aet_norm) %>% 
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

# Making a raster list of time stable variables for easy reference
time_stable <- c(cultivated,
                 aet_norm,
                 cwd_norm,
                 aet_dev,
                 cwd_dev,
                 road_dist,
                 elec_dist,
                 fveg,
                 mask) 
# Defining years of interest
years <- 2003:2016
# Making aet_norm the template
template <- aet_norm  

# Define base paths and file name patterns
raster_sources <- list(
  housing    = "mean_housing/DenMean25km_%d.tif",
  ffwi       = "fosberg/Fosberg_98_%d_gw.tif",
  time_fire  = "YearsSinceFire/YearsSinceFire_%d.tif",
  fire       = "fires/fire_%d_%d.tif"
)

# Create an empty list to hold all raster lists
all_rasters <- list()

# Read in all years of rasters for temporally dynamic variables
for (var in names(raster_sources)) {
  message("Loading ", var, " rasters...")
  
  # Use map to read rasters for each year and warp
  all_rasters[[var]] <- map(years, function(y) {
    path <- here("00_Data_Assembly", "data", sprintf(raster_sources[[var]], y, y))
    r <- read_stars(path) %>% st_warp(template) %>% st_crop(ca)
    return(r)
  })
  
  names(all_rasters[[var]]) <- as.character(years)
}

# Make empty raster for for loop
climate_raster_list <- list()

# predict fire probability for each year of interest
for (yr in years) {
  message("Processing year: ", yr) # Print message for process tracking
  
  # Build input data
  california_sf <- c( # Compiling list of all independent variable rasters
    time_stable,
    climate_mean_rasters$housing[[as.character(yr)]],
    climate_mean_rasters$ffwi[[as.character(yr)]],
    climate_mean_rasters$time_fire[[as.character(yr)]]
  ) %>%
    st_as_sf() %>% # Converting to point shapefile
    drop_na() %>% # Deleting rows with NA values
    rename( # Renaming columns to the names the model expects
      cultivated = Cultivated_Proportion_NullCorr.tif,
      housing = paste0("DenMean25km_", yr, ".tif"),
      aet_norm = aet_Norm.tif,
      cwd_norm = cwd_Norm.tif,
      aet_dev = aet2003_dev.tif,
      cwd_dev = cwd2003_dev.tif,
      road_dist = PrimSecRoads_Dist_km.tif,
      elec_dist = transmissionLines_Dist.tif,
      vegetation = fveg.tif,
      ffwi = paste0("Fosberg_98_", yr, "_gw.tif"),
      years_since_fire = paste0("YearsSinceFire_", yr, ".tif")
    ) %>%
    mutate(
      vegetation = case_when(# converting WHR10 codes to vegetation character
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
    filter(vegetation %in% c("conifer", "desert", "hardwood", "herbaceous", "shrub")) %>% # Filtering to only include vegetation of interest
    mutate(
      vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", TRUE ~ vegetation)) %>% # Combining conifer and hardwood into general tree group
    mutate(
      fire_prob = predict(ffwi_full_model, newdata = ., type = "response") # predicting fire probability
    ) %>%
    select(fire_prob, geometry) # dropping all columns but fire probability and spatial geometry 
  
  # Convert fire probability shapefile to raster using aet norm as template
  climate_prob_raster <- st_rasterize(california_sf, template = aet_norm, align = TRUE)
  
  # Store result
  climate_raster_list[[as.character(yr)]] <- climate_prob_raster
}

# Combine into time cube and compute pixel-wise mean
climate_raster_stack <- do.call(c, climate_raster_list)
names(climate_raster_stack) <- as.character(years)

climate_raster_timecube <- st_redimension(climate_raster_stack, along = list(time = as.character(years)))
mean_fire_prob <- st_apply(climate_raster_timecube, c("x", "y"), mean, na.rm = TRUE)
# Write to disk
write_stars(mean_fire_prob, here("02_WRF", "Data", "climate_mean_predict_2003_2016_average.tif"), append = F)
# Load in raster of predicted fire probability from actual data
actual_fire_prob <- read_stars(here("02_WRF", "Data", "ffwi_full_model_predict_2003_2016_average.tif"))
# Calculate climate contribution 
fire_difference <- actual_fire_prob - mean_fire_prob
# Write to disk
write_stars(fire_difference, here("02_WRF", "Data", "climate_spatial_effect.tif"))


