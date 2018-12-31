#=============================================================================#
# Name   : get_available_data
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

dirpath <- 'D:/Clorofila/crop_Peru/regrid/'
xlim <- c(-85,-70)
ylim <- c(-20,0)
zlim <- c(0,2200)

#=============================================================================#
# DO NOT CHANGE ANYTHIG AFTER HERE
#=============================================================================#
rasterFiles <- list.files(path = dirpath, pattern = '.nc', recursive = T, full.names = T)
ras         <- raster(rasterFiles[1])
mat         <- matrix(data=NA, nrow = dim(ras)[1]*dim(ras)[2], ncol = length(rasterFiles))

for (i in 1:length(rasterFiles)) {
  ras <- raster(rasterFiles[i])
  mat[,i] <- getValues(ras)
  print(rasterFiles[i])
}

mat[!is.na(mat)] <- 1
mat[ is.na(mat)] <- 0
nona <- apply(X = mat, MARGIN = c(1), FUN = sum, na.rm=T)
ras[] <- nona

png(filename = paste0(dirpath, 'availableData', '.png'), width = 850, height = 850, res = 120)
plot(ras, xlim = xlim, ylim = ylim, zlim = zlim, axes = F, legend = F, col = tim.colors(64))
grid()
axis(side = 1, font = 2, lwd.ticks = 2, cex.axis = 1.5)
axis(side = 2, font = 2, lwd.ticks = 2, cex.axis = 1.5, las = 2)
map('worldHires', add=T, fill=T, col='gray')
addRasterLegend(ras, location = c(-71,-70,-15,-3), ramp = tim.colors(64), minmax = zlim, digits = 0, cex.axis = 2)
box(lwd = 2)
dev.off()
# rm(list = ls())
#=============================================================================#
# END OF PROGRAM
#=============================================================================#