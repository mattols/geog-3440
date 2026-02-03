#
# Download monthly era5 data
#
# # # # # #

# 1. Download ERA 5 data
# install.packages(c("foreach", "ecmwfr"))
library(foreach)
library(ecmwfr)

# get API - https://cds.climate.copernicus.eu/profile?tab=profile
wf_set_key(key="enter_key_here")


# inputs
era5_variable <- "snow_depth" # "2m_temperature"
year_ <- 2025
months_ <- 1:5
base_folder <- "~/data/era5-data"
output_folder <- file.path(base_folder, era5_variable)

# file
if(!exists(output_folder)){dir.create(output_folder)}

# month list
month_list <- sprintf("%02d", months_)

# request
request <- list(
  dataset_short_name = "reanalysis-era5-land",
  product_type = "reanalysis",
  variable = "2m_temperature",
  date = "2025-01-01/2025-01-31",
  time = c(paste0("0",0:9,":00"),paste0(10:23,":00")),
  data_format = "netcdf",
  download_format = "unarchived",
  area = c(42, -112.05, 38.7, -110.5), #(N, W, S, E)
  target = "era5-temp2m-hourly-2025-01.nc"
)

file <- wf_request(
  request  = request,  # the request
  transfer = TRUE,     # download the file
  path     = "~/data/era5-test/"       # store data 
)






# # # # # # # FLH data

### WORKS!
# can do for entire month (minutes) or daily (seconds)
# more than two variables places both in a .zip folder

library(ecmwfr)

# request for pressure levels (temp & geopotential)
request <- list(
  dataset_short_name = "derived-era5-pressure-levels-daily-statistics",
  product_type = "reanalysis",
  variable = c(
    "geopotential",
    "temperature"
  ),
  year = "2025",
  month = "03",
  # day = "15",
  day = c(
    "01","02","03","04","05","06","07","08","09",
    "10","11","12","13","14","15","16","17","18",
    "19","20","21","22","23","24","25","26","27",
    "28","29","30","31"
  ),
  pressure_level = c("500", "600", "700", "850"),
  daily_statistic = "daily_mean",
  time_zone = "utc-07:00",
  frequency = "6_hourly",
  area = c(42, -112.05, 38.5, -110.5),  # North, West, South, East
  format = "netcdf",
  download_format = "unarchived",
  target = "era5_flh_ut_202503.nc"
)

# submit
fname <- wf_request(
  request = request,
  transfer = TRUE,
  path     = "~/data/era5-data/pressure-levels/"
)

# must extract .zip file if more than 1 variable

# test daily
fname <- "~/data/era5-test/era5_flh_ut_20250315/temperature_0_daily-mean.nc"        
rast(fname)

# test monthly
rast("~/data/era5-data/pressure-levels/era5_flh_ut_202503/geopotential_stream-oper_daily-mean.nc")





# # # # #
# Precipitation
# takes slightly longer since it must run at 1-hour stats
request <- list(
  dataset_short_name = "derived-era5-single-levels-daily-statistics",
  product_type = "reanalysis",
  variable = "total_precipitation",
  year = "2025",
  month = "03",
  day = c(
    "01","02","03","04","05","06","07","08","09",
    "10","11","12","13","14","15","16","17","18",
    "19","20","21","22","23","24","25","26","27",
    "28","29","30","31"
  ),
  daily_statistic = "daily_sum",
  time_zone = "utc-07:00",
  frequency = "1_hourly",
  area = c(42, -112.05, 38.5, -110.5),  # North, West, South, East
  format = "netcdf",
  download_format = "unarchived",
  target = "era5_prec_ut_202503.nc"
)

# submit
fname <- wf_request(
  request = request,
  transfer = TRUE,
  path     = "~/data/era5-data/precipitation/"
)



