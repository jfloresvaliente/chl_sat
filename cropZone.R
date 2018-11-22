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
for(year in 2002:2018){
  dirpath <- paste0('G:/Clorofila/', year, '/')
  out_path <- paste0('G:/Clorofila/crop_Peru/', year, '/')
  dir.create(path = out_path, showWarnings = F)
  # cropZone(dirpath, out_path, pre_name = sechura, xmn = -81.7, xmx = -80.7, ymn = -6, ymx = -5)
  cropZone(dirpath, out_path)
}
rm(list = ls())