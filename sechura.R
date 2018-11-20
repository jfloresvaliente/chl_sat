library(ncdf4)
library(imputeTS)
library(fields)
library(maps)
library(mapdata)

dirpath <- 'G:/Clorofila/crop_Sechura/'
chl_files <- list.files(path = dirpath, pattern = '.nc', recursive = T, full.names = T)

nc <- nc_open(chl_files[1])
chl <- ncvar_get(nc, 'chlor_a')
lon <- ncvar_get(nc, 'longitude')
lat <- rev(ncvar_get(nc, 'latitude'))

allDat <- array(dim = c(dim(chl),length(chl_files)))
for(i in 1:length(chl_files)){
  nc <- nc_open(chl_files[i])
  allDat[,,i] <- ncvar_get(nc, 'chlor_a')
  print(chl_files[i])
  nc_close(nc)
}

for(i in 1:dim(chl)[1]){
  for(j in 1:dim(chl)[2]){
    serie <- allDat[i,i,]
    
    if(sum(is.na(serie)) >= (dim(allDat)[3]-2)) next()
    
    serie <- na.interpolation(x = serie, option = 'stine')
    allDat[i,j,] <- serie
  }
}
allDat[allDat < 0] <- 0

# x11()
# image.plot(lon, rev(lat), allDat[,,281])
# map('worldHires', add=T, fill=T, col='gray')


fechas <- seq(from = as.Date('2002-01-01'), to = as.Date('2002-12-31'), by = 'days')
ini_fechas <- fechas[185]

fechas <- seq(from = ini_fechas, length.out = length(chl_files), by = 'day')

plot(fechas, allDat[1,1,], type = 'l')




fechas_ind <- NULL
for(i in 1:length(fechas)){
  a <- fechas[i]
  year <- substr(x = a, start = 1, stop = 4)
  month <- substr(x = a, start = 6, stop = 7)
  day <- substr(x = a, start = 9, stop = 10)
  fechas_ind <- rbind(fechas_ind, c(year, month, day))
}


meses <- levels(factor(fechas_ind[,2]))
clim <- array(dim = c(dim(chl),12))
x11()
par(mfrow = c(3,4), mar = c(2,2,1,2))
for(i in 1:12){
  mes <- which(fechas_ind[,2] == meses[i])
  mes3D <- allDat[,,mes]
  mes2D <- apply(mes3D, c(1,2), mean, na.rm = T)
  clim[,,i] <- mes2D
  image.plot(lon, lat, mes2D, xlab = 'Lon', ylab = 'Lat', zlim = c(0,15))
  map('worldHires', add=T, fill=T, col='gray')
  write.table(x = mes2D, file = paste0('G:/Clorofila/crop_Sechura/', 'clim', i, '.csv'),
              row.names = F, col.names = F, sep = ';')
}

write.table(x = lon, file = paste0('G:/Clorofila/crop_Sechura/','lon.csv'),
            row.names = F, col.names = F)
write.table(x = lat, file = paste0('G:/Clorofila/crop_Sechura/','lat.csv'),
            row.names = F, col.names = F)



# plot(serie, type = 'l')
# #
# 
# 
# 
# 
# 
# nc <- nc_open(chl_files[1])
# 
# 
# filename="chl_a_Sechura.nc"
# 
# xvals <- ncvar_get(nc, 'longitude')
# yvals <- ncvar_get(nc, 'latitude')
# 
# nx <- length(xvals)
# ny <- length(yvals)
# 
# lon1 <- ncdim_def("longitude", "degrees_east", xvals)
# lat2 <- ncdim_def("latitude", "degrees_north", yvals)
# 
# time <- ncdim_def("Time","days", 1:length(chl_files), unlim=TRUE)
# mv <- -999 #missing value to use
# var_temp <- ncvar_def("chlor_a", 'mg m^-3',list(lon1, lat2, time), longname="Chlorophyll Concentration, OCI Algorithm", mv) 
# 
# ncnew <- nc_create(filename, list(var_temp))
# 
# print(paste("The file has", ncnew$nvars,"variables"))
# #[1] "The file has 1 variables"
# print(paste("The file has", ncnew$ndim,"dimensions"))
# #[1] "The file has 3 dimensions"
# 
# # Some fake dataset based on latitude, to check whether the data are
# # written in the correct order
# data <- rep(yvals, each=nx)
# 
# # Add random -999 value to check whether missing values are correctly
# # written
# data[sample(1:(nx*ny), 100, replace = FALSE)] <- -999
# ncvar_put(ncnew, var_temp, data, start=c(1,1,1), count=c(nx,ny,1))
# 
# # Don't forget to close the file
# nc_close(ncnew)
# 
# # Verification
# library(rasterVis)
# out <- raster("time.nc")
# levelplot(out, margin=FALSE)
