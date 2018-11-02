#=============================================================================#
# Name   : cropZone_getTimeSerie
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
cropZone_getTimeSerie <- function(files, zones, varname = 'chlor_a'){
  # files <- list of files to read
  # matrix of [lonmin lonmax latmin latmax], for 2 or more zones, include more rows
  library(raster)
  library(ncdf4)
  
  time_serie_mat <- matrix(NA, ncol = length(files), nrow = dim(zones)[1])
  for(i in 1:length(files)){
    ras <- raster(files[i], varname = varname)
    for(jj in 1:dim(zones)[1]){
      extension <- extent(zones[jj,1], zones[jj,2], zones[jj,3], zones[jj,4])
      rc <- crop(ras, extension)
      pts <- rasterToPoints(x = rc)
      pts <- mean(pts[,3], na.rm = T)
      if (is.nan(pts)) {
        pts <- NA
      }
      time_serie_mat[jj, i] <- pts
    }
    print(files[i])
  }
  assign(x = 'Time_serie_mat', value = time_serie_mat, .GlobalEnv)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#