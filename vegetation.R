install.packages("pak")
install.packages("tidyterra")
norequire(devtools)
install_github("ilyamaclean/microclimf")
install.packages("raster")
install.packages("terra")



library(microclimf)
library(sp)
library(raster)
library(readxl)
library(dplyr)
library(terra)
library(tidyterra) 
library(ggplot2)
library(ggspatial)

# -  Track progress in R  ──────────────────────────────────────────────────────

install.packages("progress")
library(progress)

pb <- progress_bar$new(
  format = "Processing [:bar] :percent eta: :eta",
  total = 100
)

for(i in 1:100) {
  pb$tick()
  Sys.sleep(0.01)  # simulate work}

# - Vegetation parameters  ─────────────────────────────────────────────────────

                    ############## Habitat ################

# - Beginning : creation of a raster of the wood outline to use it on QGIS


wood.outline <- readRDS("CODES/wood.outline.rds")

# convert sf to SpatVector first
wood_vect <- vect(wood.outline)

# create a template raster with your desired resolution and extent
template <- rast(ext(wood_vect), resolution = 1, crs = "EPSG:27700")

# rasterize
wood_raster <- rasterize(wood_vect, template)

# export
writeRaster(wood_raster, "wood_outline.tif", overwrite = TRUE)

wood.outline_rast <- rast("Veg/wood.outline.tif")

plot(wood.outline_rast, main ='wood outline')


no#           ──────────────────────────────────────────────────────             #

# Creation of a timelapse
tme <- as.POSIXlt(c(0:8783) * 3600, origin = "2023-01-01 00:00", tz = "UTC") # 8783 hours in a year

#           ──────────────────────────────────────────────────────             #


# - 1rst step : import the habitat raster created on QGIS and create atime lapse
par(mfrow=c(1,2))

habitat <- rast("Veg/Habitats_final_version/habitat.tif")
habitat <- project(habitat, "EPSG:27700", method = "near") # method = 'near' preserves the discrete values
habitat <- project(habitat, wood.outline_rast, method = "near")


# identify cells that are inside the wood outline AND NA in habitat
fill_cells <- is.na(habitat) & !is.na(wood.outline_rast)
habitat[fill_cells] <- 4

names(habitat)
plot(habitat, main = "Habitat types in Wytham Woods")


# Assign the full data frame as levels (raster will use the label column for legends)
levels(habitat2) <- hab_levels

# First, create your habitat labels data frame
hab_levels <- data.frame(
  ID    = c(1, 4, 5, 6, 11, 12, 14, 16),
  value = c(1, 4, 5, 6, 11, 12, 14, 16),
  label = c("Evergreen needleleaf forest", "Deciduous broadleaf forest", "Mixed forest", 
            "Closed shrubland", "Tall grassland", "Permanent wetland", "Urban and built-up", 
            "Dense vegetation"))

# Load and project your habitat raster
habitat <- rast("Veg/Habitats_final_version/habitat_fv.tif")
habitat <- project(habitat, "EPSG:27700")
habitat <- project(habitat, wood.outline_rast)

# Convert to categorical and assign labels
habitat2 <- as.factor(habitat)
levels(habitat2) <- hab_levels[, c("value", "label")]

# Plot with discrete legend
colors <- c("darkgreen", "forestgreen", "lightgreen", "brown", "gold", "lightblue", "red", "orange")
plot(habitat2, main = "Habitat types in Wytham Woods", col = colors)


#           ──────────────────────────────────────────────────────             #

# - 2nd step : compute the vegetation height with LiDAR data

# Raster
dsm <- raster("LiDAR_DSM/DSM_1m.tif")  #---> vegetation height <=> hgt!!
dtm <- raster("LiDAR_DTM/DTM_1m.tif")

dsm  # 2022 values
dtm  # 2022 values

canopy <- dsm - dtm


# Terra - SpatRaster

dsm2 <- rast("LiDAR_DSM/DSM_1m.tif")
dtm2 <- rast("LiDAR_DTM/DTM_1m.tif")

dsm2  # 2022 values
dtm2  # 2022 values


# Level of accuracry of the Z values are +/- 15 cm RMSE (Root Mean Square Error)

# Few verificationsx

res(dsm2) == res(dtm2)        #   TRUE
ext(dsm2) == ext(dtm2)        #   TRUE       dsm2 <- project(d)

# Set spatial boundaries

dtm2 <- project(dtm2, wood.outline_rast,, method = "near")
dtm2 <- mask(dtm2, wood.outline_rast)
dsm2 <- project(dsm2, wood.outline_rast,, method = "near")
dsm2 <- mask(dsm2,wood.outline_rast )

# Compute the vegetation height 
canopy2 <- dsm2 - dtm2
canopy_height <- canopy2


plot(canopy_height, main = 'Canopy height (m)')

################################## 
ext(canopy_height)==ext(habitat)
[1] TRUE
 res(canopy_height)==res(habitat)
[1] TRUE TRUE

#           ──────────────────────────────────────────────────────             #


# - 3rd step : import PAI : 


# "pai' : total one sided area of both leaves, woody and dead vegetation per unit ground area
# Can be estimated from the fractional canopy cover
# Monthly canopy fraction multipliers (0-1 scale)


# Based on deciduous broadleaf woodland seasonality
monthly_cf <- c(
  0.15,  # January   - bare
  0.15,  # February  - bare
  0.40,  # March     - budbreak
  0.70,  # April     - partial leaf
  0.95,  # May       - nearly full
  0.9999,# June      - full canopy
  0.9999,# July      - full canopy
  0.9999,# August    - full canopy
  0.90,  # September - late summer
  0.60,  # October   - senescence
  0.25,  # November  - leaf fall
  0.15,  # December  - bare
  0.70   # 13th layer - annual mean
)

# Create a list of 13 PAI rasters
pai_layers <- list()


for(i in 1:13) {
  # Apply monthly fraction to canopy cover
  Cf_monthly <- ifel(canopy_height > 2, monthly_cf[i], 0)
  print(Cf_monthly)
  # Estimate PAI
  pai_monthly <- -log(1 - Cf_monthly)
  print(pai_monthly)
  # Store
  pai_layers[[i]] <- pai_monthly
}

pai_estimation <- rast(pai_layers)

# Name the layers
names(pai_estimation) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                      "Annual")


color <- c("sienna", "limegreen")
plot(pai_estimation, main='Plant Area Index approximated distribution in Wytham Woods', col = color)




#           ──────────────────────────────────────────────────────             #

# - 4th step :ESTIMATION OF SOME VEGETATION PARAMETERS

par(mfrow=c(2,2))

pai_estimation <-  unwrap(pai_estimation)
veg <- vegpfromhab(habitat, pai = pai_estimation, hgts = canopy_height, tme=tme)

veg$pai <- wrap(pai_estimation)

plot(rast(veg$pai))

plot(rast(veg$pai)[[1]], main="Vegetation height in January") 
plot(rast(veg$pai)[[5]], main="Vegetation height in May") 
plot(rast(veg$pai)[[7]], main="Vegetation height in August") 

paiarray <- as.array(veg$pai)
vegmean<-apply(paiarray, 3, mean, na.rm = TRUE)
plot(vegmean, type="l", ylim = c(0, 9), main = "Seasonal variation in PAI")

plot(rast(veg$x), main = "Leaf angle distribution") 
plot(rast(veg$gsmax), main="Max. stomatal conductance") 
plot(rast(veg$hgt), main="Canopy height") 
plot(rast(veg$clump)[[1]], main = "Jan Canopy clumping factor") # set to 0 
plot(rast(veg$leafr),col=gray(0:255/255), main = "Leaf reflectance")
plot(rast(veg$leafd), main = "Mean leaf diameter") # set to 0.05
plot(rast(veg$leaft),col=gray(0:255/255), main = "Leaf transmittance") # set equal to leafr




#           ──────────────────────────────────────────────────────             #


# - 5th step : estimate clump = how much radiation passes through gaps in the canopy : 


.is <- function(r) {
  if (class(r)[1] == "PackedSpatRaster") r<-rast(r)
  if (class(r)[1] != "matrix") {
    if (dim(r)[3] > 1) {
      y<-as.array(r)
    } else y<-as.matrix(r,wide=TRUE)
  } else y<-r
  y
}

clump_compute <- function(hgt, leafd, pai, maxclump = 0.95) {
  n <- dim(pai)[3]  # number of temporal layers
  
  # Call the function .is above
  hgt_a   <- .is(hgt)
  leafd_a <- .is(leafd)
  sel     <- which(leafd_a > hgt_a)
  lmd     <- leafd_a
  lmd[sel] <- hgt_a[sel]
  
  result <- vector("list", n)
  
  for (i in 1:n) {
    pai_i <- .is(pai[[i]])
    pai_i[pai_i > 1] <- 1
    
    clump_i <- (1 - pai_i)^(hgt_a / lmd)
    clump_i[clump_i > maxclump] <- maxclump
    
    result[[i]] <- clump_i
  }
  
  # Convert as a SpatRaster
  out <- rast(lapply(result, function(m) rast(m, crs = crs(pai), extent = ext(pai))))
  names(out) <- names(pai)
  return(out)
}

clump <- clump_compute(hgt = unwrap(veg$hgt), leafd = unwrap(veg$leafd), pai = unwrap(veg$pai), maxclump = 0.95)

clump <- wrap(clump)

veg$clump <- clump




#           ──────────────────────────────────────────────────────             #

# - 6th step : Check if NA values in veg that are not in dtm

diff_na <- is.na(resample(unwrap(veg$x), dtm, method="bilinear")) & !is.na(dtm)

# overlay on the DTM to see what terrain those gaps sit on
plot(dtm)
plot(diff_na, col = c(NA, "red"), add = TRUE, legend = FALSE)





#           ──────────────────────────────────────────────────────             #

writeRaster(x = dtm2, 'dtmcaerth.tif')
writeRaster(x = dtm2, 'dtmc_interpolate.tif')
saveRDS(veg, "veg.rds")








