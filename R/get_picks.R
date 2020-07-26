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
library(imputeTS)

dirpath <- 'D:/Clorofila/crop_Sechura/'
xmn <- -81.5
xmx <- -80.5
ymn <- -6
ymx <- -5
zlim <- c(0,15)
umbrales <- c(1,10,15,20)

#=============================================================================#
# DO NOT CHANGE ANYTHIG AFTER HERE
#=============================================================================#
xlim <- c(xmn,xmx)
ylim <- c(ymn,ymx)

# dir.create(path = paste0(dirpath, 'interp/'), recursive = T, showWarnings = F)
rasterFiles  <- list.files(path = dirpath, pattern = '.nc', recursive = T, full.names =T)
ras <- raster(rasterFiles[1])

mat <- matrix(data=NA, nrow = dim(ras)[1]*dim(ras)[2], ncol = length(rasterFiles))
for (i in 1:length(rasterFiles)) {
  miras <- raster(rasterFiles[i])
  mat[,i] <- getValues(miras)
  print(rasterFiles[i])
}

#=============================================================================#
# PLOT CHL SIN INTERPOLAR
#=============================================================================#
dfmean <- apply(mat, c(1), mean, na.rm = T)
ras[] <- dfmean

png(filename = paste0(dirpath, 'mean_chl', '.png'), width = 850, height = 850, res = 120)
par(lwd = 2)
plot(ras, xlim = xlim, ylim = ylim, zlim = zlim, axes = F, legend = F, col = tim.colors(64))
grid()
axis(side = 1, font = 2, lwd.ticks = 2, cex.axis = 1.5)
axis(side = 2, font = 2, lwd.ticks = 2, cex.axis = 1.5, las = 2)
map('worldHires', add=T, fill=T, col='gray')
addRasterLegend(ras, location = c(-71,-70,-15,-3), ramp = tim.colors(64), minmax = zlim, digits = 0, cex.axis = 2)
box(lwd = 2)
dev.off()

#=============================================================================#
# INTERPOLACION DE LAS SERIES
#=============================================================================#
NAinterp <- mat
minNA <- 10
for(i in 1:dim(NAinterp)[1]){
  if( sum(!is.na(NAinterp[i,])) > minNA ){
    NAinterp[i,] <- na.interpolation(NAinterp[i,], option = 'stine')
  }else{
    NAinterp[i,] <- NA
  }
}

#=============================================================================#
# PLOT CHL INTERP
#=============================================================================#
df <- apply(NAinterp, c(1), mean, na.rm = T)
ras[] <- df

png(filename = paste0(dirpath, 'mean_chl_interp', '.png'), width = 850, height = 850, res = 120)
par(lwd = 2)
plot(ras, xlim = xlim, ylim = ylim, zlim = zlim, axes = F, legend = F, col = tim.colors(64))
grid()
axis(side = 1, font = 2, lwd.ticks = 2, cex.axis = 1.5)
axis(side = 2, font = 2, lwd.ticks = 2, cex.axis = 1.5, las = 2)
map('worldHires', add=T, fill=T, col='gray')
addRasterLegend(ras, location = c(-71,-70,-15,-3), ramp = tim.colors(64), minmax = zlim, digits = 0, cex.axis = 2)
box(lwd = 2)
dev.off()

#=============================================================================#
# CONTABILIZAR EL NUMERO DE FLORACIONES
#=============================================================================#
get_num_picks <- function(serie){
  picks <- rle(serie)
  picks <- cbind(picks$lengths, picks$values)
  picks <- subset(picks, picks[,2] == 1)
  picks <- dim(picks)[1]
  return(picks)
}

#=============================================================================#
# PLOT FLORACIONES ALLDATA
#=============================================================================#
png(filename = paste0(dirpath, 'TotalPicksUmbrales.png'), width = 850, 850, res = 120)
par(mfrow = c(2,2), mar = c(2.5, 2.5, 1, 1), oma = c(1,1,1,.1))
for(i in umbrales){
  umbral <- i
  NUMpicks <- NAinterp
  NUMpicks[NUMpicks <  umbral] <- 0
  NUMpicks[NUMpicks >= umbral] <- 1
  
  picks_number <- apply(NUMpicks, c(1), get_num_picks)
  ras[] <- picks_number

  plot(ras, xlim = xlim, ylim = ylim, axes = F, legend = F, col = tim.colors(64))
  grid()
  axis(side = 1, font = 2, lwd.ticks = 2, cex.axis = 1.2)
  axis(side = 2, font = 2, lwd.ticks = 2, cex.axis = 1.2, las = 2)
  map('worldHires', add = T, fill = T, col = 'gray')
  mtext(text = paste('Umbral:', umbral), side = 1, line = -1.5, adj = .15, font = 2)
  addRasterLegend(ras, location = c(-71,-70,-15,-3), ramp = tim.colors(64), digits = 0, cex.axis = 2)
  box(lwd = 2)
}
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#