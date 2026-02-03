#
# Custom Functions for Avalanche Hazard Mapping
# Based on UAC rose and forecast
#

library(terra)

print("Loading custom avalanche functions")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION
UAC_forecast_hazard <- function(dem, forecast_table, min_elv_ft){
  #
  # Calculate hazard raster
  # arguments:
  #   - dem in meters
  #   - forecast table with 8 values of hazard (1-5) for different elevation zones (see UAC webpage for more)
  #   - numeric value of minimum elevation (in feet)
  # output:
  #   - raster with classified hazard zones
  #
  print("Generating terrain parameters...")
  # generate terrain parameters
  slope   <- terrain(dem, v="slope", unit="degrees")
  aspect  <- terrain(dem, v="aspect", unit="degrees")
  dem_ft  <- dem*3.281 # conversion from m to ft
  
  # parse table values into matrices
  rose_diagram    <- cbind(c(0, seq(22.5, 360, 45)), c(seq(22.5, 360, 45), 360))
  forecast_table  <- rbind(forecast_table, forecast_table[1,])
  matrix_low      <- cbind(rose_diagram, forecast_table[,1])
  matrix_mid      <- cbind(rose_diagram, forecast_table[,2])
  matrix_high     <- cbind(rose_diagram, forecast_table[,3])
  
  # generate elevation zones and classify
  elv_ranges  <- c(8000, 9500)
  print("  defining zones and classifying avalanche hazard")
  aspect_low  <- aspect; aspect_low[dem_ft > elv_ranges[1] | dem_ft < min_elv_ft] = 0
  aspect_mid  <- aspect; aspect_mid[dem_ft >= elv_ranges[2] | dem_ft <= elv_ranges[1]] = 0
  aspect_high <- aspect; aspect_high[dem_ft < elv_ranges[2]] = 0
  class_low   <- classify(aspect_low, matrix_low)
  class_mid   <- classify(aspect_mid, matrix_mid)
  class_high  <- classify(aspect_high, matrix_high)
  # combine for final hazard model
  slope_hazard  <- (slope >= 30 & slope <= 45)
  hazard_raster <- (class_low + class_mid + class_high) * slope_hazard
  # hazard_raster[slope <= 30 & slope >= 45] = 0
  hazard_raster[hazard_raster==0]=NA
  print("Hazard raster generated!")
  return(hazard_raster)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# FUNCTION
UAC_forecast_map <- function(dem, hazard_raster, hill=NULL){
  # 
  # map plot of hazard results
  # arguments:
  #   - DEM
  #   - avalanche hazard raster
  #   - (optional) hillshade - is calculated if not provided
  # output:
  #   - map plot
  #
  # define rose characteristics
  rose_cols <-  c("forestgreen", "yellow", "orange", "firebrick", "black")
  rose_levels <- c("Low", "Moderate", "Considerable", "High", "Extreme")
  haz_vals <- unique(values(hazard_raster))
  cols <- rose_cols[haz_vals[complete.cases(haz_vals)]]
  # calculate hillshade if not provided
  if(is.null(hill)){
    print("Generating hillshade for plot...")
    slope <- terrain(dem, v="slope", unit="degrees")
    aspect <- terrain(dem, v="aspect", unit="degrees")
    hill <- shade(slope*pi/180, aspect*pi/180, angle = 40, direction = 270) 
  }
  # plot
  plot(hill, col=grey(0:100/100), legend=F)
  plot(hazard_raster, col=adjustcolor(cols, 0.7), add=T, axes=FALSE, legend=FALSE)
  legend("left", title="Hazard", legend=rose_levels, title.font=2, fill=rose_cols, cex=0.7)
  print("DONE")
}