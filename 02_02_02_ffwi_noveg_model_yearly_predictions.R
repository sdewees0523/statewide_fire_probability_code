library(here)
library(tidyverse)
library(sf)
library(stars, lib = "~/Rlibs")
library(mgcv)

# Read in shapefile with all independent variables
california <- st_read(here("00_Data_Assembly", "data", "Assembled_Data", "california_all.shp"))

california_all <- california %>%
  rename(cultivated = cultvtd, # Renaming columns to the names that the model expects
         aet_norm = aet_nrm,
         cwd_norm = cwd_nrm,
         road_dist = rod_dst,
         elec_dist = elc_dst,
         vegetation = vegettn,
         years_since_fire = yrs_sn_)%>%
  filter(vegetation %in% c("conifer", "hardwood", "herbaceous", "shrub", "desert")) %>% # filtering to only include vegetation of interest
  mutate(vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", # Combining conifer and hardwood to general tree group
                                T ~ vegetation),
         vegetation = as.factor(vegetation)) # Making sure vegetation column is a factor, not character

# Reordering vegetation factor
california_all$vegetation <- factor(california_all$vegetation, levels = c("tree", "shrub", "herbaceous", "desert"))

# Reading in cultivated raster to use as template 
cultivate <- read_stars(here("00_Data_Assembly", "data", "cultivated", "Cultivated_Proportion.tif"))

# Defining years of interest
years <- 2003:2016

# For loop to read in each model and predict fire probability for the year that was excluded from the training data for that model
for (year_i in years) {
  message("Processing year: ", year_i) # Printing message for progress tracking
  
  # Load the model for year of interest
  model_path <- here("02_WRF", "models", paste0("ffwi_noveg_model_", year_i, ".rda"))
  load(model_path)  # This loads a model named like ffwi_2003_model into the environment
  
  # Dynamically get the model object (e.g., ffwi_2003_model)
  model <- get(paste0("ffwi_noveg_model_", year_i))
  
  # Predict fire probability for the year that was excluded from the training data for the current model
  pred_sf <- california_all %>%
    filter(year == year_i) %>%
    mutate(fire_prob = predict(model, newdata = ., type = "response")) %>%
    select(fire_prob) %>%
    st_as_sf()
  
  # Convert fire probability shapefile to raster using cultivated raster as a template
  pred_raster <- st_rasterize(pred_sf, template = cultivate, align = TRUE)
  
  # Write individual raster to file
  write_stars(pred_raster, here("02_WRF", 
                                "Data",
                                "yearly_model_predictions", 
                                paste0("yearly_noveg_model_prediction_", year_i, ".tif")))
}
