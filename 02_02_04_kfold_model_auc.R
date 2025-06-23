library(here)
library(tidyverse)
library(mgcv)
library(pROC)

# Loading in testing dataset (i.e. the non-poisson disk pixels, so data the model wasn't trained on)
california_test <- read.csv(here("00_Data_Assembly", "data", "ffwi_testing_disk.csv")) %>% 
  filter(vegetation %in% c("conifer", "desert", "hardwood", "shrub", "herbaceous")) %>% # Filtering to only include vegetation of interest
  mutate(vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", # Combining conifer and hardwood into general tree category
                                T ~ vegetation))

# Defining years of interest 
years <- 2003:2016
# Making empty list to use in for loop
fullmodel_list <- vector("list", length(years))

# Naming each object in list by it's corresponding year
names(fullmodel_list) <- years

## Running for the full model
# For loop to read in each model and predict fire probability for the year that was excluded from the training data for that model
for (i in seq_along(years)) {
  year_i <- years[i]
  
  # Load the model for year of interest
  model_path <- here("02_WRF", "models", paste0("ffwi_full_model_", year_i, ".rda"))
  load(model_path)  # This loads a model named like ffwi_2003_model into the environment
  
  # Dynamically get the model object (e.g., ffwi_2003_model)
  model <- get(paste0("ffwi_full_model_", year_i))
  
  # Predict for the corresponding year's test data
  preds <- california_test %>%
    filter(year == year_i) %>%
    mutate(fire_prob = predict(model, newdata = ., type = "response"))
  
  # Adding predicted values to the list created above
  fullmodel_list[[i]] <- preds
}

# Combine list from for loop into one data frame
fullmodel_all <- bind_rows(fullmodel_list)

# Performing ROC/AUC test
roc_test <- roc(fullmodel_all$fire, fullmodel_all$fire_prob)
overall_auc <- auc(roc_test)
print("Full model ROC/AUC")
print(overall_auc)

##Running for no veg model 
# Making empty list for use in for loop
noveg_model_list <- vector("list", length(years))
# Naming list object by corresponding year
names(noveg_model_list) <- years
# For loop to read in each model and predict fire probability for the year that was excluded from the training data for that model
for (i in seq_along(years)) {
  year_i <- years[i]
  
  # Load the model for year of interest
  model_path <- here("02_WRF", "models", paste0("ffwi_noveg_model_", year_i, ".rda"))
  load(model_path)  # This loads a model named like ffwi_2003_model into the environment
  
  # Dynamically get the model object (e.g., ffwi_2003_model)
  model <- get(paste0("ffwi_noveg_model_", year_i))
  
  # Predict for the corresponding year's test data
  preds <- california_test %>%
    filter(year == year_i) %>%
    mutate(fire_prob = predict(model, newdata = ., type = "response"))
  
  # Add to empty list created above
  noveg_model_list[[i]] <- preds
}

# Combine into one data frame
noveg_all <- bind_rows(noveg_model_list)

# Performing ROC/AUC test
roc_test <- roc(noveg_all$fire, noveg_all$fire_prob)
overall_auc <- auc(roc_test)
print("No Veg Model")
print(overall_auc)

##Running for noffwi no veg model 

# Creating empty list for use in for loop
noffwi_model_list <- vector("list", length(years))
# Naming each list object by corresponding year
names(noffwi_model_list) <- years
# For loop to read in each model and predict fire probability for the year that was excluded from the training data for that model
for (i in seq_along(years)) {
  year_i <- years[i]
  
  # Load the model of interest
  model_path <- here("02_WRF", "models", paste0("noffwi_noveg_model_", year_i, ".rda"))
  load(model_path)  # This loads a model named like ffwi_2003_model into the environment
  
  # Dynamically get the model object (e.g., ffwi_2003_model)
  model <- get(paste0("noffwi_noveg_model_", year_i))
  
  # Predict for the corresponding year's test data
  preds <- california_test %>%
    filter(year == year_i) %>%
    mutate(fire_prob = predict(model, newdata = ., type = "response"))
  
  # Add to list created above
  noffwi_model_list[[i]] <- preds
}

# Combine into one data frame
noffwi_all <- bind_rows(noffwi_model_list)

# Performing ROC/AUC test
roc_test <- roc(noffwi_all$fire, noffwi_all$fire_prob)
overall_auc <- auc(roc_test)
print("No FFWI No Veg Model")
print(overall_auc)

