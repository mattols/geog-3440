#
# Download hourly ERA-5 data
#
#

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

for (m in 1:length(month_list)){
  #
  # iterate over months
  
  # generate day list
  days_ <- 1:lubridate::days_in_month(as.Date(paste0(year_, m, "01"), format='%Y%m%d'))
  day_list <- sprintf("%02d", days_)
  
  for (d in 1:length(day_list)){
    #
    # iterate and write daily file
    
    request <- list(
      dataset_short_name = "reanalysis-era5-land",
      product_type = "reanalysis",
      variable = era5_variable,
      year = year_,
      month = m,
      day = d,
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
  }
  
  
}
