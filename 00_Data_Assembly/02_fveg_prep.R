library(here)
library(tidyverse)
library(raster, lib = "~/Rlibs")
## Reading in template raster for fveg dataset
cultivated <- raster(here("data", "cultivated", "Cultivated_Proportion.tif"))
## reading in fveg raster. 'RAT = TRUE' is creating the raster attribute table associated with the dataset
fveg <- raster(here("data", "fveg", "fveg_22.tif"), RAT = TRUE) 
## Making single band raster from the WHR10NUM in the raster attribute table
fveg_new <- deratify(fveg, att = "WHR10NUM")%>% 
  aggregate(fact = 33.33, fun = modal, na.rm = T) %>%  ##Aggregating the 30m resolution to 1km resolution. "fun = modal' is setting the upscaled pixel value to the most commonly accuring vegetation type in the 30m pixels that compose the new 1km pixel
  resample(cultivated, method = "ngb") # Snapping the upscaled raster to the template raster, so it aligns with all other raster datasets. 
## Writing processed fveg raster to disk.
writeRaster(fveg_new, here("data", "fveg", "fveg.tif"), overwrite = TRUE)
