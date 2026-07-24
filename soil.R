
require(devtools)
install_github("ilyamaclean/microclimf")
library(microclimf)
library(microclimdata)
library(sp)
library(raster)
library(readxl)
library(terra)
library(dplyr)

# - Soil parameters  ───────────────────────────────────────────────────────────


# - Beginning : import rasters ─────────────────────────────────────────────────

soiltype <- rast("Soil/soiltype.tif")  
groundr  <- rast("Soil/albedo.tif")
names(groundr) <- "groundr"

## Set spatial boundaries ──────────────────────────────────────────────────────

wood.outline_rast <- rast("Veg/wood.outline.tif")

soiltype <- project(soiltype, wood.outline_rast, method = "near")
soiltype <- mask(soiltype, wood.outline_rast)

groundr <- project(groundr, wood.outline_rast, method = "near")
groundr <- mask(groundr, wood.outline_rast)

# - Merge raster into one soil soil.rds file  ──────────────────────────────────

soil <- list(soiltype = soiltype, groundr = groundr)

crs(soil$soiltype)

soil <- list(soiltype = wrap(soiltype),
  groundr  = wrap(groundr))

class(soil)<- "soilcharac"

#- Fill the values that are NA in soil but not in dtm 
soil$soiltype <- rast(soil$soiltype)
fill_cells <- is.na(soil$soiltype & !is.na(wood.outline_rast))
soil$soiltype[fill_cells] <- 3
soil$soiltype <- wrap(soil$soiltype)

soil$groundr <- rast(soil$groundr)
fill_cells <- is.na(soil$groundr & !is.na(wood.outline_rast))
soil$groundr[fill_cells] <- 0.25
soil$groundr <- wrap(soil$groundr)

# Check if still NA values 
r <- resample(unwrap(soil$soiltype), dtm, method = "near")
mismatch <- is.na(r) & !is.na(dtm)
sum(as.logical(values(mismatch)), na.rm = TRUE)   # count of NA-in-soil-but-not-in-dtm cells
plot(mismatch)                                     # see where they are

r2 <- resample(unwrap(soil$groundr), dtm, method = "near")
mismatch <- is.na(r2) & !is.na(dtm)
sum(as.logical(values(mismatch)), na.rm = TRUE)  


saveRDS(soil, "soil.rds")

# Neighbouring interpolation   ─────────────────────────────────────────────────
  
    # 1_ Calcul du nombre de NA/NaN

total_cells <- ncell(soilc$soiltype)
total_nan <- sum(is.na(values(soilc$soiltype)))
(total_nan / total_cells) * 100         #       >56% !!

total_cells2 <- ncell(soilc$groundr)
total_nan2 <- sum(is.na(values(soilc$groundr)))
(total_nan2 / total_cells2) * 100         #       >63% !!

# I should decrease the resolution on QGIS!!! <======


# OR

    # Focal function : Compute a value for each cell based on its neighbours, 
#     by taking the most common value (must be integer for soiltype), or mean (for albedo) 
#     of all the valid values neighbouring (depends on the size of the matrix w)
#     treats NaN (not a number) like Na (not available)


# soilc$soiltype <- focal(soilc$soiltype,          
#                        w = 9,      # window size: 3 = 3x3 (9 cells), 5 = 5x5 (25 cells)
#                        fun = "modal",        # function applied to the window
#                        na.policy = "only",  # "only" = only fill NA cells, leave others untouched
#                        na.rm = TRUE)        # ignore NA neighbours when computing 


#OR
      # 2_ On procède par itération afin que les NA disparaissent progressivement

library(progress)


pb <- progress_bar$new(
  format = "Remplissage [:bar] :percent | NaN restants: :current | ETA: :eta",
  total = total_nan,
  clear = FALSE
)

repeat {
  nan_avant <- sum(is.na(as.matrix(soilc$soiltype)))
  
  soilc$soiltype <- focal(soilc$soiltype, w = 9, fun = "modal",
                          na.policy = "only", na.rm = TRUE)
  
  nan_apres <- sum(is.na(as.matrix(soilc$soiltype)))
  
  # Avancer la barre du nombre de NaN remplis à cette itération
  pb$tick(nan_avant - nan_apres)
  
  if (nan_apres == 0) break
}


repeat {
  nan_avant <- sum(is.na(as.matrix(albedo)))
  
  albedo <- focal(albedo, w = 9, fun = "mean",
                          na.policy = "only", na.rm = TRUE)
  
  nan_apres <- sum(is.na(as.matrix(albedo)))
  
  # Avancer la barre du nombre de NaN remplis à cette itération
  pb$tick(nan_avant - nan_apres)
  
  if (nan_apres == 0) break
}


soilc <- rast("soilc.tif")
names(soilc)

plot(soilc$soiltype, col = c('#C2A97A', '#6F6A3C', '#6B3A2A'))

plot(soilc$groundr, col = c('#C2A97A', '#6F6A3C', '#6B3A2A'))



# - Check if everything is in the right format    ──────────────────────────────

ext(soilc)
crs(soilc)

# check if it has any values
global(soilc, "notNA")  # count of non-NA cells
freq(soilc)             # frequency of each value




# - Plots : soilc[[1]] -> soiltype ; soilc[[2]] -> albedo


plot(soilc[[1]])  


plot(soilc[[2]]) 
plot(soilc[[2]], 
     main  = "Soil albedo",
     col   = rev(terrain.colors(100)),
     range = c(0, 1),
     type  = "continuous",
     legend = TRUE,
     plg   = list(title = "Reflectance (0-1)"))



# Save  ────────────────────────────────────────────────────────────────────────

writeRaster(x = soilc, 'soilc.tif', overwrite=TRUE)

writeRaster(x = soilc, 'soilc_interpolate.tif', overwrite=TRUE)

