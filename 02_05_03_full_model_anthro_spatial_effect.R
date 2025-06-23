library(here)
library(tidyverse)
library(stars, lib = "~/Rlibs")
library(mgcv)

# Loading in full model
load(file = here("02_WRF", "models", "ffwi_full_model.rda"))

# Loading in aet raster, also used as template
aet_norm <- read_stars(here("00_Data_Assembly","data","climate_normals", "aet_Norm.tif"))
# Loading in shapefile of california and making sure it is in same CRS as aet raster
ca <- st_read(here("00_Data_Assembly", "data", "ca_state", "CA_State.shp")) %>% 
  st_transform(crs = st_crs(aet_norm)) 

# Cropping aet normal to make sure it only has pixels insed California
aet_norm <- aet_norm %>% 
  st_crop(ca)

# Reading in cultivated raster, making sure it is in the same crs and snapped to aet, and cropping to only have pixels inside california
cultivated <- read_stars(here("00_Data_Assembly", "data", "cultivated", "Cultivated_Proportion_NullCorr.tif"))%>% 
  st_warp(aet_norm) %>% 
  st_crop(ca)
# Setting all values equal to 0
cultivated[[1]] <- 0

# Reading in housing density raster, making sure it is in the crs and snapped to aet, and cropping to only have pixels inside california
housing <- read_stars(here("00_Data_Assembly", "data", "mean_housing", "DenMean25km_2003.tif")) %>% 
  st_warp(aet_norm) %>% 
  st_crop(ca)
# Setting all values equal to 0
housing[[1]] <- 0
# Reading in road distance raster, making sure it is in the same crs and snapped to aet, and cropping to only have pixels inside california
road_dist <- read_stars(here("00_Data_Assembly","data","roads", "PrimSecRoads_Dist_km.tif"))%>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)
# Calculating statewide maximum value for road distance
road_dist_max <- max(road_dist[[1]], na.rm = T)
# Setting all values equal to statewide maximum
road_dist[[1]] <- road_dist_max

# Reading in electrical distance raster, making sure it is in the same crs and snapped to aet, and cropping to only have pixels inside california
elec_dist <- read_stars(here("00_Data_Assembly","data","electric", "transmissionLines_Dist.tif"))%>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)
# Calculating statewide maximum value for electrical distance
elec_dist_max <- max(elec_dist[[1]], na.rm = T)
# Setting al values to statewide maximum
elec_dist[[1]] <- elec_dist_max

# Reading in other time stable variables, making sure they are in the same crs and snapped to aet, and cropping to only have pixels inside california
cwd_norm <- read_stars(here("00_Data_Assembly","data","climate_normals", "cwd_Norm.tif"))%>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)

fveg <- read_stars(here("00_Data_Assembly","data", "fveg", "fveg.tif"))%>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)
mask <- read_stars(here("00_Data_Assembly","data", "buffer", "StatePoly_buf_water_50pct.tif")) %>% 
  st_warp(aet_norm)%>% 
  st_crop(ca)

# Combining all time stable rasters into list for easy referencing
time_stable <- c(cultivated,
                 housing,
                 aet_norm,
                 cwd_norm,
                 road_dist,
                 elec_dist,
                 fveg,
                 mask) 
# Defining years of interest
years <- 2003:2016
# Setting aet_norm as a template raster
template <- aet_norm  

# Define base paths and file name patterns
raster_sources <- list(
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
  message("Loading ", var, " rasters...")
  
  # Use map to read rasters for each year and warp
  all_rasters[[var]] <- map(years, function(y) {
    path <- here("00_Data_Assembly", "data", sprintf(raster_sources[[var]], y, y))
    r <- read_stars(path) %>% st_warp(template) %>% st_crop(ca)
    return(r)
  })
  
  names(all_rasters[[var]]) <- as.character(years)
}

# Update each year's housing density raster to 0
anthro_mean_rasters <- all_rasters

# Make empty list for for loop
anthro_raster_list <- list()

# For loop to predict yearly fire probability
for (yr in years) {
  message("Processing year: ", yr) # Print message for progress tracking
  
  # Build input data
  california_sf <- c( # Making raster list of all the independent variables
    time_stable,
    anthro_mean_rasters$aet_dev[[as.character(yr)]],
    anthro_mean_rasters$cwd_dev[[as.character(yr)]],
    anthro_mean_rasters$ffwi[[as.character(yr)]],
    anthro_mean_rasters$time_fire[[as.character(yr)]]
  ) %>%
    st_as_sf() %>% # Converting raster list to point shapefile
    drop_na() %>% # Getting rid of any rows that have NA values
    rename( # Renaming columns to the names the model expects
      cultivated = Cultivated_Proportion_NullCorr.tif,
      housing = DenMean25km_2003.tif,
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
    mutate( # Transforming WHR10 code to vegetation character
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
    filter(vegetation %in% c("conifer", "desert", "hardwood", "herbaceous", "shrub")) %>% # Filtering to only have vegetation of interest
    mutate(
      vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", TRUE ~ vegetation)) %>% # Combining conifer and hardwood into broad tree category
    mutate(
      fire_prob = predict(ffwi_full_model, newdata = ., type = "response") # Predicting fire probability
    ) %>%
    select(fire_prob, geometry) # Removing all columns but fire probability and spatial geometry
  
  # Converting point shapefile to raster using aet_norm as template
  anthro_prob_raster <- st_rasterize(california_sf, template = aet_norm, align = TRUE)
  
  # Store result in list
  anthro_raster_list[[as.character(yr)]] <- anthro_prob_raster
}

# Combine into time cube and compute pixel-wise mean
anthro_raster_stack <- do.call(c, anthro_raster_list)
names(anthro_raster_stack) <- as.character(years)

anthro_raster_timecube <- st_redimension(anthro_raster_stack, along = list(time = as.character(years)))
mean_fire_prob <- st_apply(anthro_raster_timecube, c("x", "y"), mean, na.rm = TRUE)

# Save raster of each pixel's mean value over study period to disk
write_stars(mean_fire_prob, here("02_WRF", "Data", "anthro_mean_predict_2003_2016_average.tif"), append = F)

# Load in raster of fire probability predicted from actual data
actual_fire_prob <- read_stars(here("02_WRF", "Data", "ffwi_full_model_predict_2003_2016_average.tif"))
# Calculating anthropogenic effect on fire probability 
fire_difference <- actual_fire_prob - mean_fire_prob
# Saving to disk 
write_stars(fire_difference, here("02_WRF", "Data", "anthro_spatial_effect.tif"))



