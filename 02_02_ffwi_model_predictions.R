library(here)
library(tidyverse)
library(sf)
library(stars, lib = "~/Rlibs")
library(mgcv)

# Loading in shapefile with all independent variables
california <- st_read(here("00_Data_Assembly", "data", "Assembled_Data", "california_all.shp"))

california_all <- california %>%
  rename(cultivated = cultvtd, # Renaming columns to have names the model expects
         aet_norm = aet_nrm,
         cwd_norm = cwd_nrm,
         road_dist = rod_dst,
         elec_dist = elc_dst,
         vegetation = vegettn,
         years_since_fire = yrs_sn_)%>%
  filter(vegetation %in% c("conifer", "hardwood", "herbaceous", "shrub", "desert")) %>% # Filtering to only include vegetation of interest
  mutate(vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", # Combining conifer and hardwood into general tree group
                                T ~ vegetation),
         vegetation = as.factor(vegetation)) # Making sure vegetation column is factor, not character

# Reordering vegetation factor
california_all$vegetation <- factor(california_all$vegetation, levels = c("tree", "shrub", "herbaceous", "desert"))
# Reading in cultivated raster to use as template
cultivate <- read_stars(here("00_Data_Assembly", "data", "cultivated", "Cultivated_Proportion.tif"))

# Loading in model that was trained on all years of the training data
load(here("02_WRF", "models", "ffwi_full_model.rda"))

# Predicting fire probability for all pixels and all years
california_all_predict <- california_all %>% 
  mutate(fire_prob = predict(ffwi_full_model, newdata = ., type = "response")) %>% 
  st_as_sf()

# Writing shapefile with predicted fire probability to disk
st_write(california_all_predict, here("02_WRF", "Data", "california_all_predict.shp"), append = F)

# Defining years of interest
years <- 2003:2016
#Making empty list for use in for loop
raster_list <- list()

# For loop for creating yearly raster of fire probability from above shapefile
for (year_i in years) {
  message("Processing year: ", year_i) # Print message for progress tracking
  
  # Filter for the year of interest
  pred_sf <- california_all %>%
    filter(year == year_i) %>%
    mutate(fire_prob = predict(ffwi_full_model, newdata = ., type = "response")) %>%
    select(fire_prob) %>%
    st_as_sf()
  
  # Rasterize using `cultivate` as template
  pred_raster <- st_rasterize(pred_sf, template = cultivate, align = TRUE)
  
  # Store in list
  raster_list[[as.character(year_i)]] <- pred_raster
}

# Combine into a single 3D stars object with time as a dimension
raster_stack <- do.call(c, raster_list)
names(raster_stack) <- as.character(years)
raster_timecube <- st_redimension(raster_stack, along = list(time = as.character(years)))

# Calculate mean across years
mean_raster <- st_apply(raster_timecube, c("x", "y"), mean, na.rm = TRUE)

# Save raster of average pixel value to disk
write_stars(mean_raster, here("02_WRF", "Data", "ffwi_full_model_predict_2003_2016_average.tif"))