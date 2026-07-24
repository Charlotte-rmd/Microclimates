################################################################################
#                                 Soil parameters                              #
################################################################################



# - Getting started  ───────────────────────────────────────────────────────────

library(microclimf)
library(microclimdata)
library(sp)
library(raster)
library(readxl)
library(terra)
library(dplyr)


# - Import raster files ────────────────────────────────────────────────────────

soiltype <- rast("Soil/soiltype.tif")  
groundr  <- rast("Soil/albedo.tif")
names(groundr) <- "groundr"

## Set spatial boundaries ──────────────────────────────────────────────────────

# For this, you'll need the raster file of the wood outline. 

wood.outline <- rast("wood.outline.tif")

soiltype <- project(soiltype, wood.outline, method = "near")
soiltype <- mask(soiltype, wood.outline)

groundr <- project(groundr, wood.outline, method = "near")
groundr <- mask(groundr, wood.outline)


# - Merge raster into one soil soil.rds file  ──────────────────────────────────

soil <- list(soiltype = soiltype, groundr = groundr)

soil <- list(soiltype = wrap(soiltype),
  groundr  = wrap(groundr))

class(soil)<- "soilcharac"


saveRDS(soil, "soil.rds")


# - Check if everything is in the right format    ──────────────────────────────

ext(soil)
crs(soil)


# - Check if there are NA    ────────────────────────────────────────────────────

global(soil, "notNA")  # count of non-NA cells
global(soil, "isNA")   # count of NA cells
freq(soil)             # frequency of each value




# - Plots: soilc[[1]] = soiltype ; soilc[[2]] = reflectance  ─────────────────────

plot(soil[[1]],  main  = "Soiltype")  

plot(soil[[2]], main  = "Reflectance (0-1)") 



# Save  ──────────────────────────────────────────────────────────────────────────

saveRDS(x = soil, 'soil.rds')


