#=============================================================================#
# Name   : main_chl_sat
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

dirpath <- 'D:/Clorofila/crop_Miraflores/'

rasterFiles <- list.files(path = dirpath, pattern = '.*\\.nc', full.names = T, recursive = T)
ras <- raster(rasterFiles[1]); dim(ras)

rasArray <- array(dim = c(dim(ras)[1] * dim(ras)[2], length(rasterFiles)) ); dim(rasArray)
for(i in 1:length(rasterFiles)){
  rasArray[,i] <- getValues(raster(rasterFiles[i]))
  print(rasterFiles[i])
}

# # Plot del numero de datos disponibles por pixel
# dfna <- numeric(length = dim(rasArray)[1])
# for(i in 1:dim(rasArray)[1]){
#   dfna[i] <- sum(!is.na(rasArray[i,]))
# }
# ras[] <- dfna
# x11()
# plot(ras)
# map('worldHires', add=T, fill=T, col='gray')

# Interpolacion de las series
NAinterp <- rasArray
minNA <- 2
for(i in 1:dim(NAinterp)[1]){
  if( sum(!is.na(NAinterp[i,])) > minNA ){
    NAinterp[i,] <- na.interpolation(NAinterp[i,], option = 'stine')
  }else{
    NAinterp[i,] <- NA
  }
}

# df <- apply(NAinterp, c(1), mean, na.rm = T); length(df)
# ras[] <- df
# x11()
# plot(ras, main = paste('Minimum Available Data:', minNA))
# map('worldHires', add=T, fill=T, col='gray')

# CONTABILIZAR EL NUMERO DE FLORACIONES
umbrales <- c(1,10,15,20)

get_num_picks <- function(serie){
  picks <- rle(serie)
  picks <- cbind(picks$lengths, picks$values)
  picks <- subset(picks, picks[,2] == 1)
  picks <- dim(picks)[1]
  return(picks)
}
# x11()
# par(mfrow = c(2,2))
# for(i in umbrales){
#   umbral <- i
#   NUMpicks <- NAinterp
#   NUMpicks[NUMpicks <  umbral] <- 0
#   NUMpicks[NUMpicks >= umbral] <- 1
#   
#   picks_number <- apply(NUMpicks, c(1), get_num_picks)
#   ras[] <- picks_number
#   # x11()
#   plot(ras, main = paste('Umbral:', umbral))
#   map('worldHires', add=T, fill=T, col='gray')
# }


fechas <- seq(from = as.Date('2002-01-01'), to = as.Date('2002-12-31'), by = 'days')
fechas <- seq(from = fechas[185], length.out = length(rasterFiles), by = 'days')

# fechas <- seq(from = fechas[185], length.out = 181, by = 'days')

dates <- array(dim = c(length(fechas), 3))
for(i in 1:length(fechas)){
  dates[i,] <- unlist(strsplit(x = as.character(fechas[i]), split = '-'))
}

# ------------------------#
for(year in 2002:2018){
  year_ind <- which(dates == year, arr.ind = T)[,1]
  yearDF <- NAinterp[,year_ind]; dim(yearDF)
  
  # CONTABILIZAR EL NUMERO DE FLORACIONES
  umbrales <- c(1,10,15,20)
  
  pngfile <- paste0(dirpath, year, 'floraciones.png')
  png(filename = pngfile, height = 950, width = 1050, res = 120)
  par(mfrow = c(2,2))
  for(i in umbrales){
    umbral <- i
    NUMpicks <- yearDF
    NUMpicks[NUMpicks <  umbral] <- 0
    NUMpicks[NUMpicks >= umbral] <- 1
    
    picks_number <- apply(NUMpicks, c(1), get_num_picks)
    ras[] <- picks_number
    # x11()
    plot(ras, main = paste('Umbral:', umbral, '| YEAR: ', year))
    map('worldHires', add=T, fill=T, col='gray')
  }
  dev.off()
  print(pngfile)
}


# 211-217 # dias 2002 que faltan

# # Con esto se puede bambiar la resolucion del raster
# library(raster)
# x <- raster(matrix(1:24, 4)); dim(x)
# y <- aggregate(x, fact = 2, fun = mean, na.rm = TRUE); dim(y)

# dim(x); dim(y)
# res(x); res(y)

#=============================================================================#
# END OF PROGRAM
#=============================================================================#