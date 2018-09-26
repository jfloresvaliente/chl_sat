# library(ncdf4)
# library(fields)
library(raster)

dirpath <- 'D:/Clorofila/'
nc_files <- list.files(path = dirpath, pattern = '.nc', full.names = T, recursive = T)

# # Extension Peru
# xmin <- -100
# xmax <- -70
# ymin <- -20
# ymax <- 0

# Extension Paracas
xmin <- -76.75
xmax <- -76.15
ymin <- -14.25
ymax <- -13.40

extension <- extent(xmin, xmax, ymin, ymax)
for (i in 1:length(nc_files)) {
  ras <- raster(nc_files[i], varname = "chlor_a")
  rc <- crop(ras, extension)
  rc[is.na(rc)] <- -1
  
  pts <- rasterToPoints(x = rc)
  if (i == 1) {
    lon <- pts[,1]
    lat <- pts[,2]
    time_serie <- matrix(nrow = dim(pts)[1], ncol = length(nc_files))
  }
  time_serie[,i] <- pts[,3]
  print(nc_files[i])
}
time_serie[time_serie == -1] <- NA
serie <- apply(time_serie, c(2), mean, na.rm = T)
fechas <- seq(as.Date('2002-07-04'), length.out = length(nc_files), by = 'day')
x11();plot(fechas, serie, type = 'l', xlab = '', ylab = 'chl-a')
