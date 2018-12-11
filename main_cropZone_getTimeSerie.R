#=============================================================================#
# Name   : main_cropZone_getTimeSerie
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
source('F:/GitHub/chl_sat/cropZone_getTimeSerie.R')
res <- 1/6

x11()
mask <- read.table(file = 'F:/GitHub/kinesis/data/mask_grid.csv'); mask <- as.matrix(mask)
lon  <- read.table(file = 'F:/GitHub/kinesis/data/lon_grid.csv') ; lon  <- as.matrix(lon)
lat  <- read.table(file = 'F:/GitHub/kinesis/data/lat_grid.csv') ; lat  <- as.matrix(lat)
fields::image.plot(lon,lat,mask)

pixel_off <- 15
a <- mask[-c(1:pixel_off), ] # quito 6 filas de la parte superior
b <- matrix (0 , pixel_off , ncol(a)) # crea una matriz de ceros para suplir las faltantes
c <- rbind(a,b) # suma a (con filas faltantes) y b (con las filas llenas de ceros)
mask <- mask - c # resto para tener la mascara deseada
fields::image.plot(lon,lat,mask)

lat_ind_min <- which.min(abs(lat[1,]- -20))
lat_ind_max <- which.min(abs(lat[1,]- -3))

mask[,c(1:lat_ind_min, lat_ind_max:dim(mask)[2])] = 0
fields::image.plot(lon,lat, mask)
mask[mask == 0] = NA
fields::image.plot(lon, lat, mask)
abline(h = c(-3,-20))

lon <- as.vector(lon)
lat <- as.vector(lat)
mask <- as.vector(mask)

pixelsOK <- cbind(lon, lat, mask)
pixelsOK <- subset(pixelsOK, pixelsOK[,3] ==1)
colnames(pixelsOK) <- c('lon', 'lat', 'mask'); dim(pixelsOK)

lonmin <- pixelsOK[,1] - (res/2)
lonmax <- pixelsOK[,1] + (res/2)
latmin <- pixelsOK[,2] - (res/2)
latmax <- pixelsOK[,2] + (res/2)

zones <- cbind(lonmin, lonmax, latmin, latmax)
# zones <- zones[1:10,]

dirpath <- 'G:/Clorofila/'
for(year in 2002:2010){
  subdir <- paste0(dirpath, year)
  files <- list.files(path = subdir, pattern = '.nc', full.names = T, recursive = T)
  a <- cropZone_getTimeSerie(files = files, zones = zones)
  write.table(x = a, file = paste0(dirpath, 'ChlPixelSerie_', year, '.csv'), col.names = F, row.names = F, sep = ';')
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#