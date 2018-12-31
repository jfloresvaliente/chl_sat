#=============================================================================#
# Name   : get_anom_ras
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(raster)
library(maps)
library(mapdata)
library(fields)
library(rangeBuilder)

xlim <- c(-85,-70)
ylim <- c(-20,0)
zlim <- c(-4,4)

# Get mean of all serie
dirpath <- 'D:/Clorofila/crop_Peru/regrid/'
rasterFiles <- list.files(path = dirpath, pattern = '.nc', recursive = T, full.names = T)
ras <- raster(rasterFiles[1])

meanras <- matrix(data = NA, nrow = dim(ras)[1]*dim(ras)[2], ncol = length(rasterFiles))
for(i in 1:length(rasterFiles)){
  ras <- raster(rasterFiles[i])
  meanras[,i] <- getValues(ras)
  print(rasterFiles[i])
}
meanras <- apply(meanras, c(1), mean, na.rm = T)

for (year in 2002:2018) {
  path_year <- paste0(dirpath, year)
  
  rasterFiles <- list.files(path = path_year, pattern = '.nc', recursive = T, full.names = T)
  ras <- raster(rasterFiles[1])
  meanyear <- matrix(data = NA, nrow = dim(ras)[1]*dim(ras)[2], ncol = length(rasterFiles))
  
  for(i in 1:length(rasterFiles)){
    ras <- raster(rasterFiles[i])
    meanyear[,i] <- getValues(ras)
    print(rasterFiles[i])
  }
  meanyear <- apply(meanyear, c(1), mean, na.rm = T)
  
  anom <- meanras - meanyear; range(anom, na.rm = T)
  anom[anom < -4] <- -4
  anom[anom > 4 ] <- 4
  ras[] <- anom
  
  png(filename = paste0(dirpath,'anom_chl', year, '.png'), width = 850, height = 850, res = 120)
  plot(ras, xlim = xlim, ylim = ylim, zlim = zlim, axes = F, legend = F, col = tim.colors(64))
  grid()
  axis(side = 2, font = 2, lwd.ticks = 2, cex.axis = 1.5, las = 2)
  axis(side = 1, font = 2, lwd.ticks = 2, cex.axis = 1.5)
  map('worldHires', add=T, fill=T, col='gray')
  addRasterLegend(ras, location = c(-71,-70,-15,-3), ramp = tim.colors(64), minmax = zlim, digits = 0, cex.axis = 2)
  box(lwd = 2)
  dev.off()
}
# rm(list = ls())
#=============================================================================#
# END OF PROGRAM
#=============================================================================#