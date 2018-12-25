#=============================================================================#
# Name   : get_mean_ras
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(maps)
library(mapdata)
library(fields)
library(rangeBuilder)

dirpath <- 'D:/Clorofila/crop_Peru/regrid/'
xlim <- c(-85,-70)
ylim <- c(-20,0)
zlim <- c(0,15)

for (year in 2002:2018) {
  new_path <- paste0(dirpath, year, '/')
  
  rasfiles <- list.files(path = new_path,pattern = '.nc',full.names = T,recursive = T)
  # rasfiles <- rasfiles[1:90]
  ras <- raster(rasfiles[1]);dim(ras)
  
  mat <- matrix(data=NA, nrow = dim(ras)[1]*dim(ras)[2], ncol = length(rasfiles))
  for (i in 1:length(rasfiles)) {
    miras <- raster(rasfiles[i])
    mat[,i] <- getValues(miras)
    print(rasfiles[i])
  }
  
  meanras <- apply(X = mat, MARGIN = c(1), FUN = mean, na.rm=T)
  ras[] <- meanras
  
  png(filename = paste0(dirpath, 'mean_chl',year, '.png'), width = 850, height = 850, res = 120)
  par(lwd = 2)
  plot(ras, xlim = xlim, ylim = ylim, zlim = zlim, axes = F, legend = F, col = tim.colors(64))
  grid()
  axis(side = 1, font = 2, lwd.ticks = 2, cex.axis = 1.5)
  axis(side = 2, font = 2, lwd.ticks = 2, cex.axis = 1.5, las = 2)
  map('worldHires', add=T, fill=T, col='gray')
  addRasterLegend(ras, location = c(-71,-70,-15,-3), ramp = tim.colors(64), minmax = zlim, digits = 0, cex.axis = 2)
  box(lwd = 2)
  dev.off()
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#