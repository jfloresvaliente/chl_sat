#=============================================================================#
# Name   : extract_ras_values
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(raster)
library(fields)
library(maps)
library(mapdata)
library(imputeTS)

dirpath <- 'D:/Clorofila/crop_Chimbote/'
umbrales <- c(1,10,15,20)

rasterFiles <- list.files(path = dirpath, pattern = '.*\\.nc', full.names = T, recursive = T)
ras <- raster(rasterFiles[1])

rasArray <- array(dim = c(dim(ras)[1] * dim(ras)[2], length(rasterFiles)) ); dim(rasArray)
for(i in 1:length(rasterFiles)){
  rasArray[,i] <- getValues(raster(rasterFiles[i]))
  print(rasterFiles[i])
}

#=============================================================================#
# PLOT DEL NUMERO DE DATOS DISPONIBLES POR PIXEL
#=============================================================================#
dfna <- numeric(length = dim(rasArray)[1])
for(i in 1:dim(rasArray)[1]){
  dfna[i] <- sum(!is.na(rasArray[i,]))
}
ras[] <- dfna
# x11()
png(filename = paste0(dirpath, 'AvailableData.png'), width = 850, 850, res = 120)
par(lwd = 2)
plot(ras)
map('worldHires', add=T, fill=T, col='gray')
mtext(text = 'Available Data', side = 3, line = -2, adj = .95, font = 2, cex = 2)
dev.off()

#=============================================================================#
# PLOT CHL SIN INTERPOLAR
#=============================================================================#
dfmean <- apply(rasArray, c(1), mean, na.rm = T)
ras[] <- dfmean

png(filename = paste0(dirpath, 'MeanChlAlldata.png'), width = 850, 850, res = 120)
par(lwd = 2)
plot(ras)
map('worldHires', add=T, fill=T, col='gray')
dev.off()

#=============================================================================#
# INTERPOLACION DE LAS SERIES
#=============================================================================#
NAinterp <- rasArray
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
# x11()
png(filename = paste0(dirpath, 'MeanChlAlldataInterp.png'), width = 850, 850, res = 120)
par(lwd = 2)
plot(ras)
map('worldHires', add=T, fill=T, col='gray')
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
par(mfrow = c(2,2), mar = c(2.5, 2,1, 2))
for(i in umbrales){
  umbral <- i
  NUMpicks <- NAinterp
  NUMpicks[NUMpicks <  umbral] <- 0
  NUMpicks[NUMpicks >= umbral] <- 1

  picks_number <- apply(NUMpicks, c(1), get_num_picks)
  ras[] <- picks_number
  # x11()
  plot(ras)
  map('worldHires', add=T, fill=T, col='gray')
  mtext(text = paste('Umbral:', umbral), side = 3, line = -1.5, adj = .95, font = 2)
}
dev.off()

#=============================================================================#
# PLOT PROMEDIO CHL POR PIXEL BY YEAR: 2002 - 2018
#=============================================================================#
fechas <- seq(from = as.Date('2002-01-01'), to = as.Date('2002-12-31'), by = 'days')
fechas <- seq(from = fechas[185], length.out = length(rasterFiles), by = 'days')

dates <- array(dim = c(length(fechas), 3))
for(i in 1:length(fechas)){
  dates[i,] <- unlist(strsplit(x = as.character(fechas[i]), split = '-'))
}

# ------------------------#
for(year in 2002:2018){
  year_ind <- which(dates == year, arr.ind = T)[,1]
  yearDF <- NAinterp[,year_ind]; dim(yearDF)
  
  # CONTABILIZAR EL NUMERO DE FLORACIONES
  pngfile <- paste0(dirpath, year, 'floraciones.png')
  png(filename = pngfile, height = 950, width = 1050, res = 120)
  par(mfrow = c(2,2), mar = c(2.5, 2,1, 2))
  for(i in umbrales){
    umbral <- i
    NUMpicks <- yearDF
    NUMpicks[NUMpicks <  umbral] <- 0
    NUMpicks[NUMpicks >= umbral] <- 1
    
    picks_number <- apply(NUMpicks, c(1), get_num_picks)
    ras[] <- picks_number
    # x11()
    plot(ras)
    map('worldHires', add=T, fill=T, col='gray')
    mtext(text = paste('Umbral:', umbral, '| YEAR: ', year), side = 3, line = -1.5, adj = .95, font = 2)
  }
  dev.off()
  print(pngfile)
}


# 211-217 # dias 2002 que faltan, PERO NO ESTAN DISPONIBLES EN EL SATELITE

# # Con esto se puede bambiar la resolucion del raster
# library(raster)
# x <- raster(matrix(1:24, 4)); dim(x)
# y <- aggregate(x, fact = 2, fun = mean, na.rm = TRUE); dim(y)

# dim(x); dim(y)
# res(x); res(y)

#=============================================================================#
# END OF PROGRAM
#=============================================================================#