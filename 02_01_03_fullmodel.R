library(here)
library(tidyverse)
library(mgcv)

# Loading in the training data
rtrain <- read.csv(here("00_Data_Assembly", "data", "ffwi_training_disk.csv")) %>%
  filter(vegetation %in% c("conifer", "hardwood", "herbaceous", "shrub", "desert")) %>% # Filtering to only include vegetation of interest
mutate(vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", # Combining conifer and hardwood into general tree group
                              T ~ vegetation),
       vegetation = as.factor(vegetation)) # Making sure vegetation column is a factor not character

# Reordering the vegetation factor 
rtrain$vegetation <- factor(rtrain$vegetation, levels = c("tree", "shrub", "herbaceous", "desert"))

# Defining the years of interest
years <- 2003:2016

# For loop to run a model excluding one year at a time. These models are used for a temporal k-fold validation approach 
for (year_i in years) {
  # Filter training data to exlude the year of interest
  train_data <- rtrain %>% filter(year != year_i)
  
  # Fit the model, k = 5 restricts each variable to 5 knots to minimize excessive wiggliness
  model <- gam(
    fire ~ s(cultivated, k = 5, bs = "cs") +
      s(housing, k = 5, bs = "cs") +
      s(cwd_dev, k = 5, bs = "cs") +
      s(aet_dev, k = 5, bs = "cs") +
      s(cwd_norm, k = 5, bs = "cs") +
      s(aet_norm, k = 5, bs = "cs") +
      s(years_since_fire, k = 5, bs = "cs") +
      s(road_dist, k = 5, bs = "cs") +
      s(elec_dist, k = 5, bs = "cs") +
      s(ffwi, k = 5, bs = "cs") +
      vegetation +
      s(road_dist, k = 5, by = vegetation, bs = "cs") +
      s(years_since_fire, k = 5, by = vegetation, bs = "cs") +
      s(ffwi, k = 5, by = vegetation, bs = "cs") +
      s(cwd_norm, by = vegetation, k = 5, bs = "cs") +
      s(cwd_dev, by = vegetation, k = 5, bs = "cs") +
      s(aet_norm, by = vegetation, k = 5, bs = "cs") +
      s(aet_dev, by = vegetation, k = 5, bs = "cs") +
      s(housing, by = vegetation, k = 5, bs = "cs")+
      s(cultivated, by = vegetation, k = 5, bs = "cs") +
      s(elec_dist, by = vegetation, k = 5, bs = "cs"),
    family = "binomial",
    data = train_data
  )
  # Naming model to include year of interest. This is the year the model is used to predict for
  model_name <- paste0("ffwi_full_model_", year_i)
  
  # Assigning name to current model
  assign(model_name, model)
  
  # Saving model to disk
  save(list = model_name, 
       file = here("02_WRF", "models", paste0("ffwi_full_model_", year_i, ".rda")))
  
  # Print message for progress tracking
  cat("Saved model for year", year_i, "\n")
}

# Running model trained on all the data. This model is used for partial dependency plots, mapping of variable contributions, and future predictions. 
ffwi_full_model <- gam(fire ~ s(cultivated, k = 5, bs = "cs") +
                         s(housing, k = 5, bs = "cs") +
                         s(cwd_dev, k = 5, bs = "cs") +
                         s(aet_dev, k = 5, bs = "cs") +
                         s(cwd_norm, k = 5, bs = "cs") +
                         s(aet_norm, k = 5, bs = "cs") +
                         s(years_since_fire, k = 5, bs = "cs") +
                         s(road_dist, k = 5, bs = "cs") +
                         s(elec_dist, k = 5, bs = "cs") +
                         s(ffwi, k = 5, bs = "cs") +
                         vegetation +
                         s(road_dist, k = 5, by = vegetation, bs = "cs") +
                         s(years_since_fire, k = 5, by = vegetation, bs = "cs") +
                         s(ffwi, k = 5, by = vegetation, bs = "cs") +
                         s(cwd_norm, by = vegetation, k = 5, bs = "cs") +
                         s(cwd_dev, by = vegetation, k = 5, bs = "cs") +
                         s(aet_norm, by = vegetation, k = 5, bs = "cs") +
                         s(aet_dev, by = vegetation, k = 5, bs = "cs") +
                         s(housing, by = vegetation, k = 5, bs = "cs") +
                         s(cultivated, by = vegetation, k = 5, bs = "cs") +
                         s(elec_dist, by = vegetation, k = 5, bs = "cs"),
                            family = "binomial",
                            data = rtrain)

# Saving the model trained on all the data to disk
save(ffwi_full_model,file = here("02_WRF", "models", "ffwi_full_model.rda"))
