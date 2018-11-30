#=============================================================================#
# Name   : cropZone
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
cropZone <- function(dirpath,
                     out_path = dirpath,
                     pattern = '.nc',
                     xmn=-100, xmx=-70, ymn=-20, ymx=0,
                     varname = 'chlor_a',
                     pre_name = 'peru'){

  library(raster)
  rasterFiles <- list.files(path = dirpath, pattern = pattern, full.names = F, recursive = T)
  
  for(i in 1:length(rasterFiles)){
    ras <- raster(paste0(dirpath, rasterFiles[i]), varname = varname)
    names(ras) <- varname
    zone_crop <- extent(xmn, xmx, ymn, ymx)
    ras <- crop(ras, zone_crop)
    crop_name <- paste0(out_path, pre_name, rasterFiles[i])
    writeRaster(x = ras, filename = crop_name, overwrite = TRUE)
    print(crop_name)
  }
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#
dirpath <- 'D:/Clorofila/'
pre_name <- 'ChaCoLs'
dir.create(path = paste0(dirpath, 'crop_',pre_name), showWarnings = F)
xmn <- -77.25
xmx <- -76.95
ymn <- -12.5
ymx <- -12

for(year in 2002:2018){
  dirpath_in <- paste0(dirpath, year, '/')
  out_path <- paste0(dirpath, 'crop_', pre_name, '/',year, '/')
  dir.create(path = out_path, showWarnings = F)
  
  cropZone(dirpath = dirpath_in,
           out_path = out_path,
           pre_name = tolower(pre_name),
           xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx)
  # cropZone(dirpath, out_path)
}
rm(list = ls())
