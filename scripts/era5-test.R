#
# test
# analyze ERA-5 data
# Estimate FLH after Marshall et al., 2025 - https://iopscience.iop.org/article/10.1088/1748-9326/ae2d73/meta
#

# Marshall data (2.3GB)
# https://zenodo.org/records/16846310

# tutorial for ERA 5
# https://rstudio-pubs-static.s3.amazonaws.com/1044767_d70e1b878bfe4f31ac9f0481a4178f21.html
# API changes
# https://bluegreen-labs.github.io/ecmwfr/

# hourly ERA 5 Land data
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land?tab=overview

# 1. Download ERA 5 data
# install.packages(c("foreach", "ecmwfr"))
library(foreach)
library(ecmwfr)

# get API - https://cds.climate.copernicus.eu/profile?tab=profile
wf_set_key(key="enter_key_here")

# scroll down to accept terms of use
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land?tab=download#manage-licences

# This is an example of a request as converted from 
request <- list(
  dataset_short_name = "reanalysis-era5-land",
  # dataset_short_name = "reanalysis-era5-pressure-levels",
  product_type = "reanalysis",
  # variable = "temperature",
  variable = "2m_temperature",
  year = "2024",
  month = "03",
  day = "05",
  time = "14:00",
  # pressure_level = "850",
  data_format = "netcdf",
  download_format = "unarchived",
  area = c(42, -114, 37, -109), #(N, W, S, E)
  target = "era5-temp2m-ut-03052024.nc"
)

file <- wf_request(
  request  = request,  # the request
  transfer = TRUE,     # download the file
  path     = "~/data/era5-test/"       # store data 
)


# Open NetCDF file and plot the data
# (trap read error on mac - if gdal netcdf support is missing)
file0 <- "~/data/era5-test/era5-temp2m-ut-03052024.nc"
r <- terra::rast(file0)
terra::plot(r, main = "ERA-5 Reanalysis Demo (2m Temperature)")
# maps::map("world", add = TRUE)
maps::map("state", c("Utah"), add = TRUE, col='white', lwd=2)


request <- list(
  dataset_short_name = "reanalysis-era5-land",
  product_type = "reanalysis",
  variable = "2m_temperature",
  # date = "2024-01-01/2024-04-01",
  date = "2024-01-01/2024-01-05",
  time = c(paste0("0",0:9,":00"),paste0(10:23,":00")),
  data_format = "netcdf",
  download_format = "unarchived",
  area = c(41, -112, 40, -111), #(N, W, S, E)
  target = "era5-temp2m-Wasatch1-2024-0105hr.nc"
)

file <- wf_request(
  request  = request,  # the request
  transfer = TRUE,     # download the file
  path     = "~/data/era5-test/"       # store data 
)

# visualize
# file <- "~/data/era5-test/era5-temp2m-Wasatch1-2024-01-03.nc" # daily?
# file <-  "~/data/era5-test//era5-temp2m-Wasatch1-2024-0105hr.nc"
rw <- terra::rast(file)
rw
library(terra)
plot(rw[[1:4]])

time(rw)
depth(rw)
terra::describe(rw)
terra::values(rw, mat = FALSE)[["valid_time"]]

d <- depth(rw)
d_posix <- as.POSIXct(d, origin = "1970-01-01", tz = "UTC")

time(rw) <- d_posix


dfrw <- global(rw, "mean")
dfrw$time <- d_posix
dfrw$degC <- dfrw$mean -273.15
dfrw$degF <- dfrw$degC* (9/5) +32
# OlsonNames()
dfrw$loctime <- as.POSIXct(d_posix, tz = "America/Denver")
head(dfrw)
plot(degC~loctime, data=dfrw, type='b')
abline(h=0, lty=2)
abline(v=dfrw$loctime[which.max(dfrw$degF)], col='red', lwd=0.5)


# other - daily for state of UT - default to 12 UTC (UTC-7= 5AM)
file_0103 <- "~/data/era5-test/era5-temp2m-Wasatch1-2024-01-03.nc" # daily?
r13 <- terra::rast(file_0103)
dfday <- global(r13, "mean")-273.15
dfday$time <- as.POSIXct(depth(r13), origin = "1970-01-01", tz = "UTC")
plot(mean~time, data=dfday, type='b')


mtn <- vect('~/ut-wtp/shp/GMBA_Inventory_v2.0_standard_basic/GMBA_Inventory_v2.0_standard_basic.shp')
wasatch <- mtn[grep("Wasatch", mtn$MapName), ]
maps::map('state', "Utah")
plot(wasatch, add=T)

# # # # #
# snow variables
request <- list(
  dataset_short_name = "derived-era5-single-levels-daily-statistics", 
  variable = "snow_depth",  #c("snow_albedo",
               # "snow_density", 
               # "snow_depth"),
  year  = "2025",
  month = "03",
  day   = sprintf("%02d", 1:31),
  data_format = "netcdf",
  download_format = "unarchived",
  daily_statistic = "daily_mean",
  # frequency = "1_day",
  # time_zone = "MST",
  area = c(42.1, -112.05, 38.7, -110.5), #c(41, -112, 40, -111), #(N, W, S, E)
  target = "ERA5-ut-snow-202503.nc"
)

request <- list(
  dataset_short_name = "derived-era5-land-daily-statistics",
  variable = "snow_depth", 
  year  = "2025",
  month = "03",
  day   = sprintf("%02d", 1:31),
  daily_statistic = "daily_mean",
  time_zone = "UTC",
  frequency = "1_day",
  area = c(42.1, -112.05, 38.7, -110.5),
  data_format = "netcdf",
  download_format = "unarchived",
  target = "ERA5land-snow-202503.nc"
)

request <- list(
  dataset_short_name = "derived-era5-land-daily-statistics",
  variable = "snow_depth",
  year = "2024",
  month = "03",
  day = sprintf("%02d", 1:31),
  daily_statistic = "daily_mean",
  frequency = "1_day",
  time_zone = "UTC",
  area = c(42, -112.05, 38.7, -110.5),
  data_format = "netcdf",
  download_format = "unarchived",
  target = "snow_depth.nc"
)


request <- list(
  dataset_short_name = "reanalysis-era5-land",
  product_type = "reanalysis",
  variable = "snow_depth",
  year = "2024",
  month = "03",
  day = "01",
  # day = sprintf("%02d", 1:31),
  # time = c(paste0("0",0:9,":00"),paste0(10:23,":00")),
  time = paste0(sprintf("%02d", 00:23), ":00"),
  # pressure_level = "850",
  data_format = "netcdf",
  download_format = "unarchived",
  area = c(42, -112.05, 38.7, -110.5), #(N, W, S, E)
  target = "era5-snow-ut-20240301.nc"
)

file <- wf_request(
  request  = request,  # the request
  transfer = TRUE,     # download the file
  path     = "~/data/era5-test/"       # store data 
)


sno <- terra::rast(file)
plot(sno[[1:4]])


par(mfrow=c(1,2))
maps::map("state", c("Utah"), col='black', lwd=2)
plot(sno[[31]], add=T)
plot(wasatch,add=T)



# DOES NOT WORK!
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land-timeseries?tab=download
 # # # # # # # # 
# TIMESERIES REQUESTS
# dataset = "reanalysis-era5-land-timeseries"
# request = {
#   "variable": ["2m_temperature"],
#   "location": {"longitude": -111.7, "latitude": 40.3},
#   "date": ["2024-01-01/2024-02-01"],
#   "data_format": "netcdf"
# }

# conversion
request <- list(
  dataset_short_name = "reanalysis-era5-land-timeseries",
  product_type = "reanalysis",
  variable = "2m_temperature",
  location = c(-111.7, 40.3),
  date = "2024-10-01/2025-05-01",
  data_format = "netcdf",
  target = "era5-temp2m-orem-ts.nc",
  download_format = "unarchived"
)

# this request will take some time
file_ts <- wf_request(
  request  = request,  # the request
  transfer = TRUE,     # download the file
  path     = "~/data/era5-test/"       # store data 
)



