library(here)
library(tidyverse)
library(mgcv)
library(patchwork)

# Load in training dataset 
r_train <- read.csv(here("00_Data_Assembly", "data", "ffwi_training_disk.csv")) %>%
  filter(vegetation %in% c("conifer", "hardwood", "herbaceous", "shrub", "desert")) %>% # Filter to only include vegetation of interest
  mutate(vegetation = case_when(vegetation %in% c("conifer", "hardwood") ~ "tree", # Combine conifer and hardwood into general tree category
                                T ~ vegetation),
         vegetation = as.factor(vegetation)) # Make sure vegetation column is factor, not character

# Reorder vegetation factor
r_train$vegetation <- factor(r_train$vegetation, levels = c("tree", "shrub", "herbaceous", "desert"))

# Read in full model trained on all years of training data
load(file = here("02_WRF", "models", "ffwi_full_model.rda"))
# Read in noveg model trained on all years of training data
load(file = here("02_WRF", "models", "ffwi_noveg_full_model.rda"))

# Function to generate PDP for a specific predictor across vegetation types
generate_veg_pdp <- function(predictor, veg_levels = c("tree", "shrub", "herbaceous", "desert"), 
                         model = ffwi_full_model, train_data = r_train) {
  
  # Get all model variables used (excluding response)
  model_vars <- all.vars(formula(model))[-1]
  
  # Identify numeric variables (excluding the categorical one)
  numeric_vars <- train_data %>%
    dplyr::select(all_of(model_vars)) %>%
    dplyr::select(where(is.numeric)) %>%
    names()
  
  # Generate PDP for each vegetation type
  map_dfr(veg_levels, function(veg) {
    subset <- train_data %>% filter(vegetation == veg)
    
    # Compute medians within the vegetation group
    covariate_medians <- subset %>%
      summarise(across(all_of(numeric_vars), ~ median(.x, na.rm = TRUE)))
    # Make range of values for predictor of interest spanning from first percentile to 99 percentile of data
    x_seq <- seq(quantile(subset[[predictor]], probs = 0.01, na.rm = TRUE),
                 quantile(subset[[predictor]], probs = 0.99, na.rm = TRUE),
                 length.out = 100)
    
    # Replicate median row and insert varying predictor
    newdata <- covariate_medians[rep(1, 100), ]
    newdata[[predictor]] <- x_seq
    
    # Add vegetation column back
    newdata$vegetation <- factor(veg, levels = levels(train_data$vegetation))
    
    # Make sure all columns match model input
    model_input <- model_vars
    missing_vars <- setdiff(model_input, names(newdata))
    for (var in missing_vars) {
      newdata[[var]] <- NA  # just to avoid error if anything is missing
    }
    
    # Predict
    preds <- predict(model, newdata = newdata, type = "response", se.fit = TRUE)
    
    # Add CI and metadata
    newdata %>%
      mutate(value = preds$fit,
             value_ci = preds$se.fit,
             value_lower = value - value_ci,
             value_upper = value + value_ci,
             vegetation = veg,
             predictor = predictor)
  })
}
# Run pdp model for each variable
cultivated_veg_pdp <- generate_veg_pdp("cultivated")
housing_veg_pdp <- generate_veg_pdp("housing")
cwd_dev_veg_pdp <- generate_veg_pdp("cwd_dev")
aet_dev_veg_pdp <- generate_veg_pdp("aet_dev")
cwd_norm_veg_pdp <- generate_veg_pdp("cwd_norm")
aet_norm_veg_pdp <- generate_veg_pdp("aet_norm")
ffwi_veg_pdp <- generate_veg_pdp("ffwi")
fire_veg_pdp <- generate_veg_pdp("years_since_fire")
road_veg_pdp <- generate_veg_pdp("road_dist")
elec_veg_pdp <- generate_veg_pdp("elec_dist")

# Function to generate PDP for a specific predictor for no veg model
generate_noveg_pdp <- function(predictor, 
                             model = ffwi_noveg_full_model, train_data = r_train) {
  
  # Get all model variables used (excluding response)
  model_vars <- all.vars(formula(model))[-1]
  
  # Identify numeric variables (excluding the categorical one)
  numeric_vars <- train_data %>%
    dplyr::select(all_of(model_vars)) %>%
    dplyr::select(where(is.numeric)) %>%
    names()
  
  # Generate PDP
    
    # Compute medians within the vegetation group
    covariate_medians <- train_data %>%
      summarise(across(all_of(numeric_vars), ~ median(.x, na.rm = TRUE)))
    # Make range of values for predictor of interest spanning from first percentile to 99 percentile of data
    x_seq <- seq(quantile(train_data[[predictor]], probs = 0.01, na.rm = TRUE),
                 quantile(train_data[[predictor]], probs = 0.99, na.rm = TRUE),
                 length.out = 100)
    
    # Replicate median row and insert varying predictor
    newdata <- covariate_medians[rep(1, 100), ]
    newdata[[predictor]] <- x_seq
    
    
    # Make sure all columns match model input
    model_input <- model_vars
    missing_vars <- setdiff(model_input, names(newdata))
    for (var in missing_vars) {
      newdata[[var]] <- NA  # just to avoid error if anything is missing
    }
    
    # Predict
    preds <- predict(model, newdata = newdata, type = "response", se.fit = TRUE)
    
    # Add CI and metadata
    new_data <- newdata %>%
      mutate(value = preds$fit,
             value_ci = preds$se.fit,
             value_lower = value - value_ci,
             value_upper = value + value_ci,
             predictor = predictor)
}

# Run function for all variables 
cultivated_noveg_pdp <- generate_noveg_pdp("cultivated")
housing_noveg_pdp <- generate_noveg_pdp("housing")
cwd_dev_noveg_pdp <- generate_noveg_pdp("cwd_dev")
aet_dev_noveg_pdp <- generate_noveg_pdp("aet_dev")
cwd_norm_noveg_pdp <- generate_noveg_pdp("cwd_norm")
aet_norm_noveg_pdp <- generate_noveg_pdp("aet_norm")
ffwi_noveg_pdp <- generate_noveg_pdp("ffwi")
fire_noveg_pdp <- generate_noveg_pdp("years_since_fire")
road_noveg_pdp <- generate_noveg_pdp("road_dist")
elec_noveg_pdp <- generate_noveg_pdp("elec_dist")

# Make graphs for each variable
cultivated_graph <- ggplot() +
  geom_line(data = cultivated_veg_pdp, aes(x= cultivated, y = value, color = vegetation))+
  geom_ribbon(data = cultivated_veg_pdp, aes(x = cultivated, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = cultivated_noveg_pdp, aes(x = cultivated, y = value), color = "black")+
  geom_ribbon(data = cultivated_noveg_pdp, aes(x = cultivated, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "Proportion Cultivated")

housing_graph <- ggplot() +
  geom_line(data = housing_veg_pdp, aes(x= housing, y = value, color = vegetation))+
  geom_ribbon(data = housing_veg_pdp, aes(x = housing, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = housing_noveg_pdp, aes(x = housing, y = value), color = "black")+
  geom_ribbon(data = housing_noveg_pdp, aes(x = housing, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "Housing Density")

cwd_dev_graph <- ggplot() +
  geom_line(data = cwd_dev_veg_pdp, aes(x= cwd_dev, y = value, color = vegetation))+
  geom_ribbon(data = cwd_dev_veg_pdp, aes(x = cwd_dev, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = cwd_dev_noveg_pdp, aes(x = cwd_dev, y = value), color = "black")+
  geom_ribbon(data = cwd_dev_noveg_pdp, aes(x = cwd_dev, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "CWD Deviation")

aet_dev_graph <- ggplot() +
  geom_line(data = aet_dev_veg_pdp, aes(x= aet_dev, y = value, color = vegetation))+
  geom_ribbon(data = aet_dev_veg_pdp, aes(x = aet_dev, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = aet_dev_noveg_pdp, aes(x = aet_dev, y = value), color = "black")+
  geom_ribbon(data = aet_dev_noveg_pdp, aes(x = aet_dev, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "AET Deviation")

cwd_norm_graph <- ggplot() +
  geom_line(data = cwd_norm_veg_pdp, aes(x= cwd_norm, y = value, color = vegetation))+
  geom_ribbon(data = cwd_norm_veg_pdp, aes(x = cwd_norm, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = cwd_norm_noveg_pdp, aes(x = cwd_norm, y = value), color = "black")+
  geom_ribbon(data = cwd_norm_noveg_pdp, aes(x = cwd_norm, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "CWD Normal")

aet_norm_graph <- ggplot() +
  geom_line(data = aet_norm_veg_pdp, aes(x= aet_norm, y = value, color = vegetation))+
  geom_ribbon(data = aet_norm_veg_pdp, aes(x = aet_norm, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = aet_norm_noveg_pdp, aes(x = aet_norm, y = value), color = "black")+
  geom_ribbon(data = aet_norm_noveg_pdp, aes(x = aet_norm, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "AET Normal")

ffwi_graph <- ggplot() +
  geom_line(data = ffwi_veg_pdp, aes(x= ffwi, y = value, color = vegetation))+
  geom_ribbon(data = ffwi_veg_pdp, aes(x = ffwi, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = ffwi_noveg_pdp, aes(x = ffwi, y = value), color = "black")+
  geom_ribbon(data = ffwi_noveg_pdp, aes(x = ffwi, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "Fosberg Fire Weather Index")

fire_graph <- ggplot() +
  geom_line(data = fire_veg_pdp, aes(x= years_since_fire, y = value, color = vegetation))+
  geom_ribbon(data = fire_veg_pdp, aes(x = years_since_fire, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = fire_noveg_pdp, aes(x = years_since_fire, y = value), color = "black")+
  geom_ribbon(data = fire_noveg_pdp, aes(x = years_since_fire, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "Years since fire")

road_graph <- ggplot() +
  geom_line(data = road_veg_pdp, aes(x= road_dist, y = value, color = vegetation))+
  geom_ribbon(data = road_veg_pdp, aes(x = road_dist, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = road_noveg_pdp, aes(x = road_dist, y = value), color = "black")+
  geom_ribbon(data = road_noveg_pdp, aes(x = road_dist, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "Distance to Roads")

elec_graph <- ggplot() +
  geom_line(data = elec_veg_pdp, aes(x= elec_dist, y = value, color = vegetation))+
  geom_ribbon(data = elec_veg_pdp, aes(x = elec_dist, ymin = value_lower, ymax = value_upper, color = vegetation, fill = vegetation), alpha = 0.25)+
  geom_line(data = elec_noveg_pdp, aes(x = elec_dist, y = value), color = "black")+
  geom_ribbon(data = elec_noveg_pdp, aes(x = elec_dist, ymin = value_lower, ymax = value_upper), color = "black", fill = "grey", alpha = 0.25)+
  theme_classic()+
  labs(y = "Fire Probability",
       x = "Distance to Electrical Lines")

# Combine all graphs into one figure
figure <- (cultivated_graph + housing_graph)/(cwd_dev_graph + aet_dev_graph)/(cwd_norm_graph + aet_norm_graph)/(fire_graph+ ffwi_graph)/(road_graph + elec_graph)

# Save figure
ggsave(filename = "ffwi_fullmodel_pdp.png",
       plot = figure,
       path = here("02_WRF"),
       width = 8.5,
       height = 11,
       units = "in",
       dpi = 350)
