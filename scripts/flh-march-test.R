#
# Calculate FLH
# uses fixed pressure levels and geopotential height
#
# # # # # # # # # #

# load packages
library(terra)

# Download (see monthly script)
# temp and geopotential are - pressure-levels
# precipitation is single level (surface)
# 0.25 grid
# conservative p-levels: 850, 700, 600, 500 hPa
# 850 hPa (~1500 m) (SLC ~1300 m)
# optional: 925 or 1000 hPa ? (lower valleys)
# generally do not need 850 (below surface elevation)

# load paths
base_path <- "~/data/era5-data/"
prc_path <- file.path(base_path, "precipitation", "era5_prec_ut_202503.nc")
pt_path <- file.path(base_path, "pressure-levels", 
                "era5_flh_ut_202503", "temperature_0_daily-mean.nc")
pz_path <- file.path(base_path, "pressure-levels", 
                "era5_flh_ut_202503", "geopotential_stream-oper_daily-mean.nc")

# read data
prc <- rast(prc_path) # in meters
pt <- rast(pt_path)
pz0 <- rast(pz_path)

# convert geopotential to height
pz <- pz0 / 9.80665 # (ms−2) divide by gravity to get z in meters

# surface elevation
mtn <- vect('~/ut-wtp/shp/GMBA_Inventory_v2.0_standard_basic/GMBA_Inventory_v2.0_standard_basic.shp')
wasatch <- mtn[grep("Wasatch", mtn$MapName), ]
lon_lat <- crds(centroids(terra::aggregate(wasatch)))
# 3s ~ 90 meters c(-115, -110, 40, 45) - misses lower end
dem <- elevation_3s(lon_lat[1], lon_lat[2], path=tempdir())
dem2 <- elevation_3s(lon_lat[1], lon_lat[2]-2, path=tempdir())
elvz <- terra::merge(dem, dem2)
# plot(crop(dem, wasatch[c(2,4),]))
# plot(wasatch[c(2,4),], add=T, border='white')

# NEED FULL IMAGE (mosaic)
elv2 <- resample(elv, prc[[1]])

# extract day index
# single & multi-var
pc_str <- as.numeric(gsub(".*valid_time=(\\d+)$", "\\1", names(prc)))+1
plv_str <- as.numeric(gsub(".*valid_time=(\\d+)$", "\\1", names(pz)))+1
plv_order <- as.numeric(gsub(".*pressure_level=(\\d+)_.*", "\\1", names(pz)))

## 1. Find isotherm
# Tp1 > 0 & Tp2 < 0

idx <- 19
day_levels <- pt[[plv_str==idx]]
plot(day_levels[[1]]<273.15)
plot(day_levels[[2]]<273.15)

## 2. FLH - bilinear interpolation
# T2 = mz1 + b 
# flh = z1 + ((0-T1) / (T2-T1))*(z2-z1)

## 3. weight by daily precip
# flh_weight = sum(flh_day * Pday)/ Pday


library(terra)

# Constants
zeroC <- 273.15  # Kelvin
pressure_levels <- unique(plv_order) # hPa, descending order
n_levels <- length(pressure_levels)
n_days <- nlyr(prc)

compute_FLH_day <- function(pt_day, pz_day, elv2) {
  # pt_day, pz_day: SpatRaster with 4 layers (850→500)
  
  # Function applied to each cell
  flh_cell <- function(temp_vec, z_vec) {
    # temp_vec, z_vec: vectors of length 4 (850→500)
    
    # Check if all layers < 0C
    if(all(temp_vec < zeroC, na.rm = TRUE)) {
      return(elv2_cell)  # surface elevation if fully below freezing
    }
    
    # Find first layer where temp > 0 and next < 0
    for(i in 1:(length(temp_vec)-1)) {
      if(temp_vec[i] > zeroC & temp_vec[i+1] < zeroC) {
        # Linear interpolation of height to 0°C
        t1 <- temp_vec[i]; t2 <- temp_vec[i+1]
        z1 <- z_vec[i]; z2 <- z_vec[i+1]
        
        flh <- z1 + (zeroC - t1) * (z2 - z1)/(t2 - t1)
        return(flh)
      }
    }
    
    # If temp never crosses zero (e.g., all > 0 or other edge case)
    return(NA)
  }
  
  # Apply cell-wise using terra::app
  # Note: elv2_cell is a per-cell constant → can pass via 'filename' trick
  flh_day <- app(c(pt_day, pz_day), fun=function(v) {
    n <- length(v)/2
    temp_vec <- v[1:n]
    z_vec <- v[(n+1):(2*n)]
    elv2_cell <- v[1] * 0 + NA  # placeholder, will set later
    flh_cell(temp_vec, z_vec)
  })
  
  return(flh_day)
}


# Initialize output list
FLH_list <- vector("list", n_days)

for(d in 1:n_days) {
  # Extract 4 layers for this day
  idx <- ((d-1)*n_levels + 1):(d*n_levels)
  pt_day <- subset(pt, idx)
  pz_day <- subset(pz, idx)
  
  # Compute FLH
  FLH_list[[d]] <- compute_FLH_day(pt_day, pz_day, elv2)
}

# Combine into a raster stack
FLH_rast <- rast(FLH_list)
names(FLH_rast) <- paste0("FLH_day", 1:n_days)


# weighting by precip
FLH_weighted <- FLH_rast * prc  # element-wise multiplication
# To get monthly mean:
FLH_month_mean <- app(FLH_weighted, fun=function(x) sum(x, na.rm=TRUE)/sum(values(prc), na.rm=TRUE))




#
#
#
#
#

library(terra)

# -----------------------
# Constants
# -----------------------
zeroC <- 273.15       # 0°C in Kelvin
pressure_levels <- c(850, 700, 600, 500)  # hPa descending
n_levels <- length(pressure_levels)
n_days <- 31          # number of days in March

# -----------------------
# Helper function: compute FLH for a single cell
# -----------------------
flh_cell <- function(temp_vec, z_vec, surf_elev) {
  # temp_vec, z_vec: vectors of length = number of layers (descending pressure)
  # surf_elev: scalar surface elevation
  
  # Case 1: all layers below 0°C → FLH = surface
  if(all(temp_vec < zeroC, na.rm = TRUE)) {
    return(surf_elev)
  }
  
  # Case 2: find first bracketing layer (T>0 above, T<0 below)
  for(i in 1:(length(temp_vec)-1)) {
    if(temp_vec[i] > zeroC & temp_vec[i+1] < zeroC) {
      t1 <- temp_vec[i]
      t2 <- temp_vec[i+1]
      z1 <- z_vec[i]
      z2 <- z_vec[i+1]
      
      flh <- z1 + (zeroC - t1) * (z2 - z1)/(t2 - t1)
      
      # Make sure FLH is at least surface elevation
      return(max(flh, surf_elev))
    }
  }
  
  # Case 3: no crossing found → return NA
  return(NA)
}

# -----------------------
# Main loop: compute daily FLH
# -----------------------
FLH_list <- vector("list", n_days)

for(d in 1:n_days) {
  # Extract 4 layers for this day
  idx <- ((d-1)*n_levels + 1):(d*n_levels)
  pt_day <- subset(pt, idx)
  pz_day <- subset(pz, idx)
  
  # Stack temp + geopotential
  temp_and_z <- c(pt_day, pz_day, elv2)
  
  # Apply cell-wise
  FLH_day <- app(temp_and_z, fun=function(v) {
    # Split vectors
    temp_vec <- v[1:n_levels]
    z_vec <- v[(n_levels+1):(2*n_levels)]
    surf_elev <- v[2*n_levels + 1]
    
    flh_cell(temp_vec, z_vec, surf_elev)
  })
  
  FLH_list[[d]] <- FLH_day
}

# Combine into a raster stack
FLH_rast <- rast(FLH_list)
names(FLH_rast) <- paste0("FLH_day", 1:n_days)

# -----------------------
# Optional: precipitation-weighted FLH
# -----------------------
# Ensure prc has same extent/resolution
if(!is.null(prc)) {
  FLH_weighted <- FLH_rast * prc
  FLH_month_mean <- app(FLH_weighted, fun=function(x) sum(x, na.rm=TRUE)/sum(values(prc), na.rm=TRUE))
}

# -----------------------
# Done: FLH_rast contains daily FLH
# FLH_month_mean contains precipitation-weighted monthly FLH
# -----------------------



# FLH_rast: daily FLH raster (one layer per day)
# elv2: surface elevation raster

# Create a logical raster stack: TRUE = elevation below FLH
snow_mask <- elv2 >= FLH_rast
names(snow_mask) <- paste0("snow_mask_day", 1:nlyr(FLH_rast))
snow_month1 <- app(snow_mask, fun=sum, na.rm=TRUE)/nlyr(snow_mask)

# OPTION - calculate snow receiving area per day
# Count how many grid cells are below FLH each day
snow_area <- global(snow_mask, fun="sum", na.rm=TRUE)
# Multiply by grid cell area if you want actual km²
cell_area_km2 <- prod(res(elv2)) * (111^2)  # approximate: 1 deg ~111 km
snow_area_km2 <- snow_area * cell_area_km2

# OPTION - mask precipitation to calculate snowfall
# prc: daily precipitation raster (same extent)
snow_precip <- prc
for(i in 1:nlyr(prc)) {
  snow_precip[[i]] <- mask(prc[[i]], snow_mask[[i]], maskvalue=FALSE)
}
monthly_snow <- app(snow_precip, fun=sum, na.rm=TRUE)

# plot (in meters)
plot(monthly_snow)
plot(wasatch, add=T, border='white')


# test amount of precip
snopt <- vect(cbind(-111.62, 
                    40.43),
              crs="epsg:4326")
extract(monthly_snow, snopt)*1000
# 68 mm

# PRISM
prec_prism03 <- rast("~/data/ut-wtp/prism/prism_ppt_us_30s_202503/prism_ppt_us_30s_202503.tif")
ppc <- crop(prec_prism03, wasatch)
plot(ppc)
