# library(ncdf4)
# dirpath <- 'G:/Clorofila/crop_Peru/'
# 
# for(i in 2002:2018){
#   new_path <- paste0(dirpath, i)
#   nc_files <- list.files(path = new_path, pattern = '.nc', full.names = T, recursive = T)
#   
#   ras_array <- array(dim = c(480,720,length(nc_files)))
#   for(j in 1:length(nc_files)){
#     nc <- nc_open(nc_files[j])
#     ras_array[,,j] <- ncvar_get(nc, 'chlor_a')
#     nc_close(nc)
#     print(nc_files[j])
#   }
#   save(ras_array, file = paste0(dirpath, i, '.Rdata'))
# }

dirpath <- 'G:/Clorofila/crop_Peru/'
library(imputeTS)

nc_files <- list.files(path = dirpath, pattern = '.nc', full.names = T, recursive = T)
load(paste0(dirpath, '2002.Rdata'))
dimX <- dim(ras_array)[1]
dimY <- dim(ras_array)[2]
#









ras_array[is.na(ras_array[,,1])] <- 0.00001
ras_array[is.na(ras_array[,,dim(ras_array)[3]])] <- 0.00001

library(fields)
x11(); image.plot(ras_array[,,174])

dat <- apply(ras_array, c(1,2), na.interpolation, option ='stine')

