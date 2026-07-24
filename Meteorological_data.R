
################################################################################
#          Extracting meteorological coarse grid data from ERA5                #
################################################################################


# ──────────── packages to install and related libraries to upload ─────────────

install.packages(c("sf", "ggplot2", "terra", "tidyterra", "ncdf4", "tidyverse"))
remotes::install_github("dklinges9/mcera5")

library(ncdf4)
library(terra)
library(readr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)  
library(ggplot2)
library(mcera5)
library(ecmwfr)



# ─────────────── mcera5 extraction of meteorological data ─────────────────────


# Set your CDS credentials once (register at cds.climate.copernicus.eu)
wf_set_key(key = "Key-number")  

# Wytham Woods coordinates
lon <- -1.338
lat <-  51.773

# Build the request

req <- build_era5_request(
  xmin = -1.6,   # W
  xmax = -1.0,   # E
  ymin =  51.5,  # S
  ymax =  52.0,  # N
  start_time = as.POSIXct("2021-10-01", tz = "UTC"),
  end_time   = as.POSIXct("2025-12-31", tz = "UTC"),
  outfile_name = "era5_data"
)
request_era5(request  = req,
             out_path = "./era5_past_data/")

# Merge the files : 

nc_files <- list.files("path/", 
                       pattern = "era5_data_202*\\.nc$",
                       full.names = TRUE)


# Generate all year-month combinations 2023-2025

years <- 2023
months <- 1:12

clim_list <- list()

# Extract the required data in the right format (microclimf)

for (yr in years){
  for (mo in months) {
    
    nc_file <- paste0("./path/era5_data_", yr, "_", mo, ".nc")
    
    if (!file.exists(nc_file)) next
    
    start <- as.POSIXct(paste0(yr, "-", sprintf("%02d", mo), "-01 00:00:00"), tz = "UTC")
    end   <- start %m+% months(1) - hours(1)
    
    message("Extracting: ", yr, "_", mo)
    
    clim_list[[paste(yr, mo, sep = "_")]] <- extract_clim(
      nc         = nc_file,
      long       = lon,
      lat        = lat,
      start_time = start,
      end_time   = end,
      format     = "microclimf")
  }
}


# Bind all months into one dataframe

clim_full <- dplyr::bind_rows(clim_list)

# Check if there are any NA values 

cat("NA in temp:", sum(is.na(clim_full$temp)), "\n")
cat("NA in relhum:", sum(is.na(clim_full$relhum)), "\n")
cat("NA in pres:", sum(is.na(clim_full$pres)), "\n")
cat("NA in swdown:", sum(is.na(clim_full$swdown)), "\n")
cat("NA in lwdown:", sum(is.na(clim_full$lwdown)), "\n")
cat("NA in windspeed:", sum(is.na(clim_full$windspeed)), "\n")
cat("NA in windir:", sum(is.na(clim_full$windir)), "\n")
cat("NA in precip:", sum(is.na(clim_full$precip)), "\n")


# I encountered issues with some values <0 for shortwave radiation and diffuse radiation
# In order to fix it : 

# Shortwave radiation
clim_full$swdown[which(clim_full$swdown <0)] <- 0

# difrad
clim_full$difrad[which(clim_full$difrad<0)] <- 0


# ───────────────────────────── Saving ─────────────────────────────────

write.csv(clim_full, "climdata_hourly.csv", row.names = FALSE)




















