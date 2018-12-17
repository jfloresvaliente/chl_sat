#=============================================================================#
# Name   : cropZone
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    :
#=============================================================================#
cropZone <- function(
  input_path,
  output_path = input_path,
  pattern = '.nc',
  xmn=-100, xmx=-70, ymn=-20, ymx=0,
  varname = 'chlor_a',
  pre_name = 'peru')
  {

  library(raster)
  rasterFiles <- list.files(path = input_path, pattern = paste0('.*\\',pattern), full.names = F, recursive = T)
  
  for(i in 1:length(rasterFiles)){
    ras <- raster(paste0(input_path, rasterFiles[i]), varname = varname)
    names(ras) <- varname
    zone_crop <- extent(xmn, xmx, ymn, ymx)
    ras <- crop(ras, zone_crop)
    crop_name <- paste0(output_path, pre_name, rasterFiles[i])
    writeRaster(x = ras, filename = crop_name, overwrite = TRUE)
    print(crop_name)
  }
}
#=============================================================================#
# END OF FUNCTION
#=============================================================================#
dirpath <- 'D:/Clorofila/'
library(maps)
library(mapdata)

# #-----PERU DOMAIN-----#
# pre_name <- 'Peru'
# xmn <- -90
# xmx <- -70
# ymn <- -20
# ymx <- 0

# #-----PERU DOMAIN-----#
# pre_name <- 'Peru4'
# xmn <- -90
# xmx <- -70
# ymn <- -20
# ymx <- -15

# #-----SECHURA DOMAIN-----#
# pre_name <- 'Sechura'
# xmn <- -82
# xmx <- -80
# ymn <- -7
# ymx <- -4

# #-----MIRAFLORES DOMAIN-----#
# pre_name <- 'Miraflores'
# xmn <- -77.3
# xmx <- -77
# ymn <- -12.25
# ymx <- -11.9

# #-----CHIMBOTE DOMAIN-----#
pre_name <- 'Chimbote'
xmn <- -78.8
xmx <- -78.4
ymn <- -9.4
ymx <- -8.9

# DO NOT CHANGE ANYTHIG AFTER HERE #
new_folder <- paste0(dirpath, 'crop_', pre_name)
dir.create(path = new_folder, showWarnings = F)

png(filename = paste0(new_folder, '/cropDomain.png'), width = 850, height = 850, res = 120)
map('worldHires', add=F, fill=T, col='gray', ylim = c(ymn, ymx), xlim = c(xmn, xmx))
axis(1)
axis(2, las = 2)
box()
dev.off()

for(year in 2002:2018){
  input_path <- paste0(dirpath, year, '/')
  output_path <- paste0(new_folder, '/',year, '/')
  dir.create(path = output_path, showWarnings = F)
  
  cropZone(input_path = input_path,
           output_path = output_path,
           pre_name = tolower(pre_name),
           xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx)
}
rm(list = ls())
#=============================================================================#
# END OF PROGRAM
#=============================================================================#