library(fields)
library(sp)
dirpath <- 'D:/Herve/output/'
gridpath <- dirpath

lon <- as.matrix(read.table(paste0(gridpath, 'lon_grid.txt')))
lon <- lon[,1]
lat <- as.matrix(read.table(paste0(gridpath, 'lat_grid.txt')))
lat <- as.numeric(lat[1,])
# mask <- as.matrix(read.table(paste0(gridpath, 'mask_grid.csv')))
# mask[mask == 0] <- NA

# files <- list.files(path = dirpath, pattern = 'mean', full.names = T)

# chl <- NULL
# for(i in files){
#   dat <- read.table(i)
#   chl <- cbind(chl, dat[,3])
# }
# chl <- apply(chl, c(1), mean)

# chl <- matrix(data = chl, nrow = dim(mask)[1], ncol = dim(mask)[2])
chl <- read.table(file = paste0(dirpath,'meanChl2013.csv'), header = F)
# image.plot(lon, lat, chl*mask)

latitudes <- c(-5,-10,-15,-20)

png(filename = 'C:/Users/ASUS/Desktop/ChlMean2013Satelital.png', height = 850, width = 850, res = 120)
par(mfrow = c(2,2))
for(i in latitudes){
  nlat <- i
  latrow <- which(x = lat == nlat, arr.ind = T)
  
  row_serie <- chl[,latrow]
  row_serie[is.na(row_serie)] <- 0
  # plot(lon, row_serie, type = 'l', ylab = 'Chl mg/m^3', main = paste('LAT', nlat))
  
  coast_init <- which.max(row_serie == 0) - 1
  coast_serie <- row_serie[1:coast_init]
  coast_lon <- lon[1:coast_init]
  # plot(coast_lon, coast_serie, type = 'l', ylab = 'Chl mg/m^3', main = paste('LAT', nlat))
  
  disMat <- matrix(c(coast_lon, rep(nlat, length(coast_lon))), nrow = length(coast_lon), ncol = 2)
  coast_distance <- spDistsN1(pts = disMat, pt= disMat[1,], longlat = T)
  
  plot(rev(coast_distance),coast_serie, type = 'l',
       xlab = 'Distancia a la Costa', ylab = 'Chl mg/m^3',
       xlim = c(0,400), ylim = c(0,10), main = paste('LAT', nlat))
}
dev.off()
