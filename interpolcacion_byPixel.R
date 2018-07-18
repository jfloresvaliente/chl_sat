# ===============================================================================
# Name   : 
# Author : 
# Date   : 
# Version: 
# Aim    : 
#===============================================================================
library(imputeTS)
library(mapdata)
library(ncdf4)
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
  chl_array_interp <- array(dim = c(dim(file1), length(chl_files)))
  for(i in 1:dim(file1)[1]){
    for(j in 1:dim(file1)[2]){
      x0 <- sum(is.na(chl_array[i,j,])) # AQui debo contar en numero de Na para saber si es posible interpolar
      if((length(chl_files)-x0) >=2){
        x0 <- chl_array[i,j,] # AQui debo contar en numero de Na para saber si es posible interpolar
        inter_vec <- na.interpolation(x = x0, option = 'stine')
        inter_vec[inter_vec < 0] = 0
        chl_array_interp[i,j,] <- inter_vec
      }else{
        chl_array_interp[i,j,] <- rep(NA, length(chl_array[i,j,]))
      }
    }
  }
  chl_array_interp <- round(x = chl_array_interp, digits = 3)
  
  # Create a ncfile
  ncpath <- dirpath
  ncname <- paste0('surface_chl', Y)  
  ncfname <- paste(ncpath, ncname, '.nc', sep='')
  dname <- 'chl'
  
  # create and write the netCDF file -- ncdf4 version
  # define dimensions
  londim  <- ncdim_def('lon' ,'degrees',as.double(lon)) 
  latdim  <- ncdim_def('lat' ,'degrees',as.double(lat)) 
  timedim <- ncdim_def('time','days'   ,as.double(1:length(chl_files)))
  
  # define variables
  fillvalue <- NA
  dlname <- 'surface satellite chlorophyll mg/m^3'
  chl_def <- ncvar_def('chl','mg/m^3',list(londim,latdim,timedim), fillvalue, dlname, prec='single')

  # create netCDF file and put arrays
  ncout <- nc_create(ncfname,list(chl_def),force_v4=TRUE)
  
  # put variables
  ncvar_put(ncout,chl_def,chl_array_interp)
  
  # put additional attributes into dimension and data variables
  ncatt_put(ncout,'lon','axis','X') #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,'lat','axis','Y')
  ncatt_put(ncout,'time','axis','T')
  
  ncout
  nc_close(ncout)
}
#===============================================================================
# END OF PROGRAM
#===============================================================================