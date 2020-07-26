#=============================================================================#
# Name   : regrid_raster
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(raster)
dirpath <- 'D:/Clorofila/crop_Sechura/'
year_in <- 2003
year_on <- 2019
fact    <- 3 # dimesion en que se debe 'agregar'

#=============================================================================#
# DO NOT CHANGE ANYTHIG AFTER HERE
#=============================================================================#
for (year in year_in:year_on) {
  dir.create(path = paste0(dirpath, 'regrid/', year), recursive = T, showWarnings = F)
  newpath      <- paste0(dirpath, year, '/')
  rasterFiles  <- list.files(path = newpath, pattern = '.nc', recursive = F, full.names =F)
  
  for (i in 1:length(rasterFiles)) {
    ras     <- raster(paste0(newpath, rasterFiles[i]))
    ras2    <- aggregate(ras, fact = fact, fun = mean, na.rm = T)
    newname <- paste0(dirpath, 'regrid/', year, '/', rasterFiles[i])
    writeRaster(x = ras2, filename = newname, overwrite = TRUE)
    print(newname)
  }
}
rm(list = ls())
#=============================================================================#
# END OF PROGRAM
#=============================================================================#