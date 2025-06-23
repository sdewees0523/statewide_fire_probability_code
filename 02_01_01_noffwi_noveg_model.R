library(here)
library(tidyverse)
library(mgcv)

# Reading in training data
rtrain <- read.csv(here("00_Data_Assembly", "data", "ffwi_training_disk.csv")) %>%
  filter(vegetation %in% c("conifer", "hardwood", "herbaceous", "shrub", "desert")) %>% # Filtering to only include vegetation of interest
  mutate(vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", # Combining conifer and hardwood into general tree dominated category
                                T ~ vegetation),
         vegetation = as.factor(vegetation)) # Making sure vegetation column is a factor, not character

# Reordering the levels of vegetation factor
rtrain$vegetation <- factor(rtrain$vegetation, levels = c("tree", "shrub", "herbaceous", "desert"))

# Defining years of interest
years <- 2003:2016

# For loop to run a model excluding one year at a time. These models are used for a temporal k-fold validation approach 
for (year_i in years) {
  # Filter training data to exclude year of interest
  train_data <- rtrain %>% filter(year != year_i)
  
  # Fit the model, k = 5 restricts the knots to 5 to minimize excessive wiggliness
  model <- gam(
    fire ~ s(cultivated, k = 5, bs = "cs") +
      s(housing, k = 5, bs = "cs") +
      s(cwd_dev, k = 5, bs = "cs") +
      s(aet_dev, k = 5, bs = "cs") +
      s(cwd_norm, k = 5, bs = "cs") +
      s(aet_norm, k = 5, bs = "cs") +
      s(years_since_fire, k = 5, bs = "cs") +
      s(road_dist, k = 5, bs = "cs") +
      s(elec_dist, k = 5, bs = "cs"),
    family = "binomial",
    data = train_data
  )
  
  # Setting model name to include the year of data that was excluded. This is the year this model will be used to predict for. 
  model_name <- paste0("noffwi_noveg_model_", year_i)
  
  # Assinging the model name to the current model
  assign(model_name, model)
  # Save the model to file
  save(list = model_name, 
       file = here("02_WRF", "models", paste0("noffwi_noveg_model_", year_i, ".rda")))
  
  # Print message for progress tracking
  cat("Saved model for year", year_i, "\n")
}
