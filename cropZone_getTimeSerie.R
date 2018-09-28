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
  
  time_serie_mat <- matrix(NA, ncol = dim(zones)[1], nrow = length(files))
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
      time_serie_mat[i, jj] <- pts
    }
    print(files[i])
  }
  assign(x = 'Time_serie_mat', value = time_serie_mat, .GlobalEnv)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#

dirpath <- 'D:/Clorofila/'
files <- list.files(path = dirpath, pattern = '.nc', full.names = T, recursive = T)
files <- files[1:10]
zones <- matrix(c(-81.22, -80.7, -5.89, -5.18,  # Sechura
                  -76.75, -76.15, -14.25, -13.40 # Paracas
                  ), ncol = 4, byrow = T)

a <- cropZone_getTimeSerie(files = files, zones = zones)
