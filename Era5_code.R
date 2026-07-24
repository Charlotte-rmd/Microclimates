

install.packages(c("sf", "ggplot2", "terra", "tidyterra", "viridis"))
install.packages("ncdf4") 
install.packages("tidyverse")
library(ncdf4)
library(terra)
library(readr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)  

# for the difrad
source("https://raw.githubusercontent.com/ilyamaclean/microctools/master/R/solar.R")
exists("difprop")


################################################################################
#                        Radcliff Observatory                                  #
################################################################################

radcliff <- read.csv("weather_era5_and_radcliff.data/Radcliffe-daily-data-to-Dec-2025.csv")
View(radcliff)


radcliff <- radcliff[radcliff$YYYY>2021,]
radcliff <- radcliff[radcliff$YYYY<2026,]
colnames(radcliff)[colnames(radcliff) == "Daily.Tmean..C" ] <- "temp1"
colnames(radcliff)[colnames(radcliff) == "Rainfall.mm.raw.incl.traces"] <- "precip1"

times <- as.POSIXct(paste(radcliff$YYYY, radcliff$MM, radcliff$DD, sep = "-"),
                    format = "%Y-%m-%d", tz = "UTC")

radcliff$obs.time<- as.POSIXct(paste(radcliff$YYYY, radcliff$MM, radcliff$DD, sep = "-"),
                       format = "%Y-%m-%d", tz = "UTC")

radcliff <- radcliff %>%
  select(obs.time, temp1, precip1)


radcliff <- radcliff %>%
  mutate(precip1 = na_if(precip1, "tr"),
         precip1 = as.numeric(precip1))

radcliff <- radcliff %>% filter(obs.time >= "2023-01-01" & obs.time <= "2025-12-31")

cat("Total NAs per column:\n")
print(colSums(is.na(radcliff)))
# 2 NA for tempertaure and 271 'tr' (= trace values) for precipitation


#_ Extreme values
dates_extreme_precip <- radcliff %>%
  filter(precip1 > 2.5, !is.na(precip1))

dates_extreme_precip

dates_extreme_temp <- radcliff %>%
  filter(precip1 > 16, !is.na(temp1))

dates_extreme_temp


#- Plot
radcliff_long <- radcliff %>%
  pivot_longer(cols = -obs.time, names_to = "variables", values_to = "values")



radcliff_graph<- radcliff_long %>%
  filter(variables %in% c("temp1", "precip1")) %>%
  ggplot(aes(x = obs.time, y = values, colour = variables)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ variables, scales = "free_y", ncol = 1,
             labeller = labeller(variables = c(
               temp1   = "Temperature (°C)",
               precip1 = "Precipitation (mm)"))) +
  scale_colour_manual(values = c(
    temp1   = "#e05c2a",
    precip1 = "#2a7ae0")) +
  labs(title = "Wytham Woods — Daily Radcliff Observatory Variables (2022–2025)",
       x = NULL, y = NULL, colour = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))


radcliff_graph



# Comparison with the temperature from the thermologgers

library(csv2)
thermolog <- read.csv("Desktop/Internship_Oxford/CODES/themologger_dec.csv")

library(dplyr)

# calculates the average temperature by date for each box 
daily_means <- thermolog %>%
  group_by(year, box, day) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE))




################################################################################
#                                ERA-5                                        #
################################################################################

# ── Time axis (from any file, all share the same valid_time) ──────────────────
nc <- nc_open("weather_era5_and_radcliff.data/era5_2m-temperature.nc")

clim_full$times   <- as.POSIXct(time_raw * 3600, origin = "1970-01-01 00:00:00", tz = "UTC")

clim_full$dates    <- as.Date(clim_full$obs_time)

################ mcera5 extraction of mmeteorological data #####################

remotes::install_github("dklinges9/mcera5")
library(mcera5)
library(ecmwfr)


# Set your CDS credentials once (register at cds.climate.copernicus.eu)
wf_set_key(key = "34c881cc-5d1a-4975-a99c-812a59fd80d3")

# Wytham Woods coordinates
lon <- -1.338
lat <-  51.773

# Build the request
req <- build_era5_request(
  xmin = -1.6,   # bounding box W
  xmax = -1.0,   # E
  ymin =  51.5,  # S
  ymax =  52.0,  # N
  start_time = as.POSIXct("2021-10-01", tz = "UTC"),
  end_time   = as.POSIXct("2022-12-31", tz = "UTC"),
  outfile_name = "wytham_era5_pastdata"
)
request_era5(request  = req,
             out_path = "./era5_past_data/")

# Merge the files : 

nc_files <- list.files("./era5_past_data/", 
                       pattern = "wytham_era5_pastdata_2020_*\\.nc$",
                       full.names = TRUE)


# Generate all year-month combinations 2023-2025
years <- 2020
months <- 1:12

clim_list <- list()

for (yr in years){
  for (mo in months) {
    
    nc_file <- paste0("./era5_past_data/wytham_era5_pastdata_", yr, "_", mo, ".nc")
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

# Verif 


cat("NA in temp:", sum(is.na(clim_full$temp)), "\n")
cat("NA in relhum:", sum(is.na(clim_full$relhum)), "\n")
cat("NA in pres:", sum(is.na(clim_full$pres)), "\n")
cat("NA in swdown:", sum(is.na(clim_full$swdown)), "\n")
cat("NA in lwdown:", sum(is.na(clim_full$lwdown)), "\n")
cat("NA in windspeed:", sum(is.na(clim_full$windspeed)), "\n")
cat("NA in windir:", sum(is.na(clim_full$windir)), "\n")
cat("NA in precip:", sum(is.na(clim_full$precip)), "\n")


#PB :  valeurs <0 :  

# Shortwave radiation
clim_full$swdown[which(clim_full$swdown <0)] <- 0

# difrad
clim_full$difrad[which(clim_full$difrad<0)] <- 0



# Creation of the dataframe
write.csv(clim_full, "climdata_hourly.csv", row.names = FALSE)

################################ Climdata #######################################

climdata <- tibble(
  obs_time =  as.POSIXct(clim_full$obs_time, tz = "UTC"),
  # Temperature & humidity
  temp = clim_full$temperature,                 # °C
  relhum = clim_full$relhum,                    # %
  # Pressure 
  pres  = clim_full$pressure/1000,      # kPa   
  # Radiation                         # W/m2
  swdown = (clim_full$rad_dni + clim_full$raddr)*3600 ,
  difrad = clim_full$rad_dif * 3600,
  lwdown = clim_full$downlong * 3600,
  # Wind
  windspeed = clim_full$wind_speed,          # m/s
  winddir = clim_full$wind_direction,      # °
  # Precipitation
  precip = total_precipitation * 1000 )      # mm






# tr stands for "trace"
climdata <- climdata %>%
  mutate(precip = na_if(precip, "tr"),
         precip = as.numeric(precip))





























################### Compute mean annual air temperature ########################

# Compute
Annual_mean_T <- clim_mf$temp %>%
  group_by(year) %>%
  summarise(mean_temp <- mean(temp), .groups = "drop")

#Graph
wood.outline <- rast("wood.outline.tif")
location <- read.csv("CODES/known_nestboxes_x_y.csv")
location <- location %>%
  select(Latitude, Longitude)


loggers <- data.frame(location, mean_temp)
loggers_sf <- st_as_sf(loggers, coords = c("lon", "lat"), crs = 4326)




# ── Build hourly data frame ───────────────────────────────────────────────────
df_hourly <- tibble(
  datetime             = dates,
  obs_time                 = times,
  # Temperature & humidity
  temp        = temp_C,
  dewpoint_C   = dew_C,
  relhum = RH,
  # Pressure & precipitation
  pres  = surface_pressure/1000,
  precipitation_m      = total_precipitation,
  precip     = total_precipitation * 1000,
  # Radiation (J/m² per hour → divide by 3600 for W/m²)
  shortwave_rad_Jm2    = shortwave_flux,
  longwave_rad_Jm2     = longwave_flux,
  swdown    = shortwave_flux / 3600,
  lwdown     = longwave_flux  / 3600,
  # Wind
  u10               = u10,
  v10               = v10,
  windspeed        = wind_speed,
  winddir   = wind_direction)

# Check how many tr/NA values there were
sum(is.na(df_hourly$precip))


climdata <- df_hourly %>%
  dplyr::select(obs_time, temp, relhum, pres, swdown, difrad, lwdown, windspeed, winddir, precip)

class(climdata$obs_time)
attributes(climdata$obs_time)

write.csv(climdata, "climdata.csv", row.names = FALSE)

df_hourly <- weather_data_hourly

# ── Sanity checks ─────────────────────────────────────────────────────────────
cat("\n--- Hourly dataframe ---\n")
print(head(df_hourly))
cat("\nDimensions:", nrow(df_hourly), "rows ×", ncol(df_hourly), "cols\n")
cat("Date range:", format(min(times)), "→", format(max(times)), "\n")
cat("Total NAs per column:\n")
print(colSums(is.na(df_hourly)))


#- Compiling daily data

df_daily <- df_hourly %>%
  mutate(obs_time = as_date(obs_time)) %>%
  select(obs_time, temp, precip) %>%
  group_by(obs_time) %>%
  summarise(
    across(-c(precip), ~ mean(.x, na.rm = TRUE)),
    precip = sum(precip, na.rm = TRUE),
    .groups = "drop")

df_daily_long <- df_daily %>%
  group_by(date = as.Date(obs.time)) %>%
   summarise(
    temp          = mean(temp, na.rm = TRUE),
    precip       = sum(precip, na.rm = TRUE),
    pres       = mean(pres, na.rm = TRUE),
    relhum  = mean(relhum, na.rm = TRUE)) %>%
  
  pivot_longer(cols = -date, names_to = "variable", values_to = "value")

#- Extreme values

#_ Extreme values
dates_extreme_precip_era <- df_daily %>%
  filter(precip1 > 2.614427, !is.na(precip))

dates_extreme_precip_era

dates_extreme_temp_era <- df_daily %>%
  filter(precip1 > 15,252, !is.na(temp))

dates_extreme_temp_era



# _ Plot
era <- ggplot(df_daily_long, aes(x = date, y = value, colour = variable)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1,
             labeller = labeller(variable = c(
               temp          = "Temperature (°C)",
               precip       = "Precipitation (mm)",
               pres    = "Surface Pressure (Pa)",
               relhum  = "Relative Humidity (%)"))) +
  scale_colour_manual(values = c(
    temp         = "#e05c2a",
    precip      = "#2a7ae0",
    pres   = "#6a2ae0",
    relhum = "#2ae0a0")) +
  
  labs(title = "Wytham Woods — Daily ERA5 Variables (2022–2025)",
       x = NULL, y = NULL, colour = NULL) +
  
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

era


era2 <- df_daily_long %>%
  filter(variable %in% c("temp", "precip")) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1,
             labeller = labeller(variable = c(
               temp   = "Temperature (°C)",
               precip = "Precipitation (mm)"))) +
  scale_colour_manual(values = c(
    temp   = "#e05c2a",
    precip = "#2a7ae0")) +
  labs(title = "Wytham Woods — Daily ERA5 Variables (2022–2025)",
       x = NULL, y = NULL, colour = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

era2



################################################################################
#                   Comparison between the two predictions                     #
################################################################################

library(patchwork)
era2/radcliff_graph

write.

# Temperature
ggplot() +
  geom_line(data = df_daily, aes(x = obs.time, y = temp, colour = "Era-5 database")) +
  geom_line(data = radcliff, aes(x = obs.time, y = temp1, colour = "Radcliff Observatory database")) +
  scale_colour_manual(values = c("Era-5 database" = "#f05a2e", "Radcliff Observatory database" = "#c06e7a")) +
  labs(title = "Daily Mean Temperature",
       x = NULL, y = "Temperature (°C)", colour = "Site") +
  theme_minimal()

# Statistic tests

# 1° normality 

# Shapiro-Wilk (best for n < 5000)
shapiro.test(df_daily$temp)
shapiro.test(radcliff$temp1)
#-- not normal (not the relevant model)


# For larger datasets (n > 5000) use Kolmogorov-Smirnov
ks.test(df_daily$temp, "pnorm", mean(df_daily$temp, na.rm=TRUE), sd(df_daily$temp, na.rm=TRUE))


ks.test(radcliff$temp1, "pnorm", mean(radcliff$temp1, na.rm=TRUE), sd(radcliff$temp1, na.rm=TRUE))

#-- normal ! 

# Visual check 
qqnorm(df_daily$temp)
qqline(df_daily$temp, col = "red")

ggplot(df_daily, aes(x = temp)) +
  geom_histogram(bins = 40, fill = "#e05c2a", alpha = 0.7) +
  geom_density(aes(y = after_stat(count) * (max(df_daily$temp) - min(df_daily$temp)) / 40),
               colour = "black") +
  labs(title = "Distribution of daily temperature") +
  theme_minimal()

qqnorm(radcliff$temp1)
qqline(radcliff$temp1, col = "red")

ggplot(radcliff, aes(x = temp1)) +
  geom_histogram(bins = 40, fill = "#e05c2a", alpha = 0.7) +
  geom_density(aes(y = after_stat(count) * (max(radcliff$temp1) - min(radcliff$temp1)) / 40),
               colour = "black") +
  labs(title = "Distribution of daily temperature") +
  theme_minimal()



# 2° Equal variance

var.test(df_daily$temp, radcliff$temp1)


# 3° Compare means 
t.test(df_daily$temp, radcliff$temp1, var.equal = FALSE)

##### Significantly different

#_ Bias 
nrow(df_daily)
nrow(radcliff)


bias_df <- tibble(
  date     = df_daily$obs_time,
  abs_bias = (df_daily$temp - radcliff$temp1))

ggplot(bias_df, aes(x = date, y = abs_bias)) +
  geom_line(colour = "#e05c2a", linewidth = 0.4) +
  geom_smooth(method = "loess", colour = "black", linewidth = 0.6) +
  geom_hline(yintercept = mean(bias_df$abs_bias, na.rm = TRUE),
             linetype = "dashed", colour = "#2a7ae0") +
  labs(title = "Relative Bias — ERA5 vs Radcliff Temperature",
       subtitle = paste("Mean bias:", round(mean(bias_df$abs_bias, na.rm = TRUE), 2), "°C"),
       x = NULL, y = "Absolute Bias (°C)") +
  theme_minimal()

comparison <- radcliff %>%
  left_join(df_daily, by = c("obs.time" = "obs_time"))


# calcul des stats globales (toutes données confondues)
stats_all <- comparison %>%
  summarise(
    rmse = sqrt(mean((temp1 - temp)^2, na.rm = TRUE)),
    r2   = cor(temp1, temp, use = "complete.obs")^2,
    bias = mean(temp1 - temp, na.rm = TRUE)
  ) %>%
  mutate(label = paste0("RMSE ", round(rmse,1), "\nR\u00b2 ", round(r2,2), "\nBias ", round(bias,1)))


library(ggplot2)

ggplot(comparison, aes(x = temp, y = temp1)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  annotate(
    "text",
    x = -Inf, y = Inf,          # top-left corner
    hjust = -0.1, vjust = 1.2,
    label = stats_all$label,
    size = 4
  ) +
  labs(
    x = "ERA5 temperatures",
    y = " Radcliff temperatures") +
  theme_minimal()


### Mean bias = 0.86°C for era5 predictions

# _ Normality



#_ Equal variances

# Precipitation

## Outliers ?
> radcliff[which.max(radcliff$precip1), ]
obs.time temp1 precip1
76602 2024-09-22 17.15    77.1

> radcliff[which(radcliff$precip1>40), ]
obs.time temp1 precip1
76602 2024-09-22 17.15    77.1
76603 2024-09-23 14.55    41.8r

> df_daily2[which.max(df_daily2$precip), ]
obs.time    temp precip
  1 2025-11-14  11.6   45.6

#- Statistics
  
ks.test(df_daily$precip, "pnorm", mean(df_daily$precip, na.rm=TRUE), sd(df_daily$precip, na.rm=TRUE))
  
ks.test(radcliff$precip1, "pnorm", mean(radcliff$precip1, na.rm=TRUE), sd(radcliff$precip1, na.rm=TRUE))
  
  #-- NOT normal ! 
  
  # Visual check 
  qqnorm(df_daily$precip)
  qqline(df_daily$precip, col = "red")
  
  
  qqnorm(radcliff$precip1)
  qqline(radcliff$precip1, col = "red")
  
#_ not normal
  

  # 2° Compare means 
wilcox.test(df_daily$temp, radcliff$temp1)
  
  ##### Significantly different
  
  
  #_ Bias 
  nrow(df_daily)
  nrow(radcliff)
  
# A tibble is just a modern version of a data.frame in R
  
bias_precip_df <- tibble(
    date         = df_daily$obs.time,
    abs_bias     = abs(df_daily$precip - radcliff$precip1))
  
ggplot(bias_precip_df, aes(x = date, y = abs_bias)) +
    geom_line(colour = "#2a7ae0", linewidth = 0.4) +
    geom_smooth(method = "loess", colour = "black") +
    geom_hline(yintercept = mean(bias_precip_df$abs_bias, na.rm = TRUE),
               linetype = "dashed", colour = "#e05c2a") +
    labs(title = "Absolute Bias — ERA5 vs Radcliff Precipitation",
         subtitle = paste("Mean bias:", round(mean(bias_precip_df$abs_bias, na.rm = TRUE), 2), "mm"),
         x = NULL, y = "Absolute Bias (mm)") +
    theme_minimal()

  
  
# Plot
  
  ggplot() +
    geom_line(data = df_daily, aes(x = obs.time, y = temp, colour = "Era-5 database")) +
    geom_line(data = radcliff, aes(x = obs.time, y = temp1, colour = "Radcliff Observatory database")) +
    scale_colour_manual(values = c("Era-5 database" = "#f05a2e", "Radcliff Observatory database" = "#c06e7a")) +
    labs(title = "Daily Mean Temperature",
         x = NULL, y = "Temperature (°C)", colour = "Site") +
    theme_minimal()
  
  
  

  
################################################################################
#               Comparison between era5 1 thermologgers data                   #
################################################################################

thermolog <- read_csv("~/Desktop/Internship_Oxford/CODES/themologger_dec.csv")
View(thermolog)


thermolog$obs.time<- as.POSIXct(paste(thermolog$year, thermolog$month, thermolog$day, sep = "-"),
                               format = "%Y-%m-%d", tz = "UTC")

temp2 <- df_daily %>%
  select(obs.time, temp) %>%
  filter(obs.time >= as.Date("2023-01-01"),
         obs.time <= as.Date("2024-12-31"))
  

ggplot() +
  geom_line(data = temp2, aes(x = obs.time, y = temp, colour = "Era-5 database")) +
  geom_line(data = thermolog2, aes(x = obs.time, y = mean_temp, colour = "Thermologgers")) +
  scale_colour_manual(values = c("Era-5 database" = "blue", "Thermologgers" = "yellow")) +
  labs(title = "Daily Mean Temperature",
       x = NULL, y = "Temperature (°C)", colour = "Site") +
  theme_minimal()

  
  
## Old version : 


# Compute the diffuse radiation with the shorwave radiation data and the function difprop

df_hourly$jd <- as.numeric(format(df_hourly$datetime, "%j"))
df_hourly$hour <- as.numeric(format(df_hourly$obs_time, "%H"))

df_hourly$difrad <- difprop(
  rad = df_hourly$swdown,
  jd = df_hourly$jd,
  localtime = df_hourly$hour,
  lat = 51.77,
  long = -1.34,
  hourly = TRUE,
  watts = TRUE,
  merid = round(-1.34/15, 0) * 15,
  dst = 0,
  corr = 1)

  
  
  
  
  
  
  
