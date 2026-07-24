################################################################################
#                              Vegetation parameters                           #
################################################################################

# -  Getting started ───────────────────────────────────────────────────────────

library(microclimf)
library(sp)
library(raster)
library(readxl)
library(dplyr)
library(terra)
library(tidyterra) 
library(ggplot2)


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

# A -  Habitat -

# - Beginning : creation of a raster of the wood outline 

wood.outline <- rast("wood.outline.tif")
  

# - 1rst step : import the habitat raster created on QGIS and create a time lapse
par(mfrow=c(1,2))

habitat <- rast("vegetation_types.tif")
habitat <- project(habitat, wood.outline, method = "near")


# identify cells that are inside the wood outline AND NA in habitat and assign
# them  to the Deciduous broadleaf forest category (number 4).
  
fill_cells <- is.na(habitat) & !is.na(wood.outline_rast)
habitat[fill_cells] <- 4     

# Assign the full data frame as levels (raster will use the label column for legends)

hab_levels <- data.frame(
  ID    = c(1, 4, 5, 6, 11, 12, 14, 16),
  value = c(1, 4, 5, 6, 11, 12, 14, 16),
  label = c("Evergreen needleleaf forest", "Deciduous broadleaf forest", "Mixed forest", 
            "Closed shrubland", "Tall grassland", "Permanent wetland", "Urban and built-up", 
            "Dense vegetation"))

  levels(habitat) <- hab_levels[, c("value", "label")]


# Plot with discrete legend
  
colors <- c("darkgreen", "forestgreen", "lightgreen", "brown", "gold", "lightblue", "red", "orange")
plot(habitat, main = "Habitat types in Wytham Woods", col = colors)



# - Compute the vegetation height with LiDAR data ────────────────────────────────────────────────

# Raster
  
dsm <- raster("LiDAR_DSM/DSM_1m.tif")
dtm <- raster("LiDAR_DTM/DTM_1m.tif")
  


# Few checks (reproject if not True)

res(dsm) == res(dtm)        
ext(dsm) == ext(dtm)       

#  if not True

dtm <- project(dtm, wood.outline,, method = "near")
dtm <- mask(dtm, wood.outline)
dsm <- project(dsm, wood.outline,, method = "near")
dsm <- mask(dsm,wood.outline )

# Compute the vegetation height 
  
canopy_height <- dsm - dtm

plot(canopy_height, main = 'Canopy height (m)')


# Few checks (reproject if not True)

ext(canopy_height)==ext(habitat)
res(canopy_height)==res(habitat)


# * BIAS * 
# - Esstimation of the Plant Area Index (PAI)  ───────────────────────────────────────────


# "pai' : total one sided area of both leaves, woody and dead vegetation per unit ground area
# Can be estimated from the fractional canopy cover
# Monthly canopy fraction multipliers (0-1 scale)

# Monthly vegetation cover based on deciduous broadleaf woodland seasonality

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

  
# Create a list of 13 PAI rasters (the 13th is the annual mean)
  
pai_layers <- list()

for(i in 1:13) {
  # Apply monthly fraction to canopy cover
  Cf_monthly <- ifel(canopy_height > 2, monthly_cf[i], 0)
  print(Cf_monthly)
  # Estimate PAI
  pai_monthly <- -log(1 - Cf_monthly)          # from ......
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




# Other parameters with vegfromhab() ──────────────────────────────────────────────────────


pai_estimation <-  unwrap(pai_estimation)
veg <- vegpfromhab(habitat, pai = pai_estimation, hgts = canopy_height, tme=tme)
veg$pai <- wrap(pai_estimation)

  
plot(rast(veg$pai))
plot(rast(veg$pai)[[1]], main="Vegetation height in January") 
plot(rast(veg$pai)[[5]], main="Vegetation height in May") 
plot(rast(veg$pai)[[7]], main="Vegetation height in August") 

plot(rast(veg$x), main = "Leaf angle distribution") 
plot(rast(veg$gsmax), main="Max. stomatal conductance") 
plot(rast(veg$hgt), main="Canopy height") 
plot(rast(veg$clump)[[1]], main = "Jan Canopy clumping factor") # set to 0 
plot(rast(veg$leafr),col=gray(0:255/255), main = "Leaf reflectance")
plot(rast(veg$leafd), main = "Mean leaf diameter") # set to 0.05
plot(rast(veg$leaft),col=gray(0:255/255), main = "Leaf transmittance") # set equal to leafr




# - Estimate vegetation clumpiness ('clump' parameter) ──────────────────────────────────────────


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


# - Final checks  ─────────────────────────────────────────────────────────────────────

# - 6th step : Check if NA values in veg that are not in dtm

diff_na <- is.na(resample(unwrap(veg$x), dtm, method="bilinear")) & !is.na(dtm)

# overlay on the DTM to see what terrain those gaps sit on
                     
plot(dtm)
plot(diff_na, col = c(NA, "red"), add = TRUE, legend = FALSE)




# - Save dtm as raster and veg as rds files ──────────────────────────────────────────

writeRaster(dtm, 'dtm.tif')
saveRDS(veg, "veg.rds")








