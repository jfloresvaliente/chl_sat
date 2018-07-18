# ===============================================================================
# Name   : 
# Author : 
# Date   : 
# Version: 
# Aim    : 
#===============================================================================
library(imputeTS)
library(mapdata)
library(fields)
dirpath <- 'D:/Herve/output/regrid/'

lon <- read.table(paste0(dirpath, 'lon_grid.txt'))[,1]
lat <- as.numeric(read.table(paste0(dirpath, 'lat_grid.txt'))[1,])

for(Y in 2002:2015){
  year <- Y
  chl_files <- list.files(path = dirpath, pattern = paste0('A',as.character(year)), full.names = T)
  file1 <- read.table(file = chl_files[1], header = F)
  
  chl_array <- array(dim = c(dim(file1), length(chl_files)))
  for(i in 1:length(chl_files)){
    dat <- as.matrix(read.table(file = chl_files[i], header = F))
    chl_array[,,i] <- dat
    print(chl_files[i])
  }
  
  # Contar el numero de datos faltantes NA
  chl_array_NA <- array(dim = c(dim(file1)))
  chl_array_noNA <- array(dim = c(dim(file1)))
  for(i in 1:dim(file1)[1]){
    for(j in 1:dim(file1)[2]){
      x0 <- sum(is.na(chl_array[i,j,]))
      x1 <- sum(!is.na(chl_array[i,j,])) 
      chl_array_NA[i,j] <- x0
      chl_array_noNA[i,j] <- x1
    }
  }
  
  PNG1 <- paste0(dirpath, 'NAinData', year,'.png')
  png(filename = PNG1, width = 850, height = 850, res = 120)
  image.plot(lon, lat, chl_array_NA, main = paste('NA in data', year, 'Data Available', length(chl_files)), zlim = c(0, length(chl_files)))
  map('worldHires', add=T, fill=T, col='gray')#, ylim = ylimmap, xlim = xlimmap)
  dev.off()
  
  PNG2 <- paste0(dirpath, 'noNAinData', year,'.png')
  png(filename = PNG2, width = 850, height = 850, res = 120)
  image.plot(lon, lat, chl_array_noNA, main = paste('NA in data', year, 'Data Available', length(chl_files)), zlim = c(0, length(chl_files)))
  map('worldHires', add=T, fill=T, col='gray')#, ylim = ylimmap, xlim = xlimmap)
  dev.off()
}
#===============================================================================
# END OF PROGRAM
#===============================================================================