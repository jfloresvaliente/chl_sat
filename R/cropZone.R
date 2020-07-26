#=============================================================================#
# Name   : cropZone
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Cortar zonas definidas de la forma [lonmin, lonmax, latmin, latmax]
# URL    :
#=============================================================================#
cropZone <- function(
  input_path,
  output_path = input_path,
  pattern = '.nc',
  xmn= -100,
  xmx= -70,
  ymn= -20,
  ymx= 0,
  varname = 'chlor_a',
  pre_name = 'peru'
  ){
  #============ ============ Arguments ============ ============#
  
  # input_path  = Directorio donde se ubican los archivos de entrada
  # output_path = Directorio donde guardaran los archivos cortados
  # pattern = Patron de busqueda para los archivos
  # xmn = longitud minima
  # xmx = longitud maxima
  # ymn = latitud minima
  # ymx = latitud maxima
  # varname = nombre de la variable del archivo raster
  # pre_name = pre-nombre para los archivos de salida

  #============ ============ Arguments ============ ============#
  library(raster)
  rasterFiles <- list.files(path = input_path, pattern = paste0('.*\\',pattern), full.names = F, recursive = T)
  
  for(i in 1:length(rasterFiles)){
    ras         <- raster(paste0(input_path, rasterFiles[i]), varname = varname)
    names(ras)  <- varname
    zone_crop   <- extent(xmn, xmx, ymn, ymx)
    ras         <- crop(ras, zone_crop)
    outname     <- paste0(output_path, pre_name, rasterFiles[i])
    writeRaster(x = ras, filename = outname, overwrite = TRUE)
    print(outname)
  }
}
#=============================================================================#
# END OF FUNCTION
#=============================================================================#
library(maps)
library(mapdata)

dirpath <- 'D:/Clorofila/'
year_in <- 2003
year_on <- 2018

# #-----PERU DOMAIN-----#
# pre_name <- 'Peru'
# xmn <- -85
# xmx <- -70
# ymn <- -20
# ymx <- 0

# #-----AFRICA NW DOMAIN-----#
# pre_name <- 'AfricaNW'
# xmn <- -25
# xmx <- -10
# ymn <- 4
# ymx <- 30

# #-----PERU DOMAIN by LAT-----#
# pre_name <- 'Peru4'
# xmn <- -90
# xmx <- -70
# ymn <- -20
# ymx <- -15

# #-----SECHURA DOMAIN-----#
# pre_name <- 'Sechura'
# xmn <- -82
# xmx <- -80
# ymn <- -7
# ymx <- -4

#-----SECHURA DOMAIN-----#
pre_name <- 'Sechura'
xmn <- -81.5
xmx <- -80.5
ymn <- -6
ymx <- -5

# #-----MIRAFLORES DOMAIN-----#
# pre_name <- 'Miraflores'
# xmn <- -77.3
# xmx <- -77
# ymn <- -12.25
# ymx <- -11.9

# #-----CHIMBOTE DOMAIN-----#
# pre_name <- 'Chimbote'
# xmn <- -78.8
# xmx <- -78.4
# ymn <- -9.4
# ymx <- -8.9

# #-----CHERREPE DOMAIN-----#
# pre_name <- 'Cherrepe'
# xmn <- -80
# xmx <- -79.5
# ymn <- -7.33
# ymx <- -7.01

#=============================================================================#
# DO NOT CHANGE ANYTHIG AFTER HERE
#=============================================================================#
new_folder <- paste0(dirpath, 'crop_', pre_name)
dir.create(path = new_folder, showWarnings = F)

#========== Plot map of  cropDomain ==========#
png(filename = paste0(new_folder, '/cropDomain.png'), width = 850, height = 850, res = 120)
map('worldHires', add=F, fill=T, col='gray', xlim = c(xmn, xmx), ylim = c(ymn, ymx), mar = c(3.5,8,1,1))
grid()
axis(side = 1, font = 2, lwd.ticks = 2, cex.axis = 1.5)
axis(side = 2, font = 2, lwd.ticks = 2, cex.axis = 1.5, las = 2)
box(lwd = 2)
dev.off()

#========== Bucle para leer cada carpeta dentro de dirpath ==========#
for(year in year_in:year_on){
  input_path  <- paste0(dirpath, year, '/')
  output_path <- paste0(new_folder, '/',year, '/')
  dir.create(path = output_path, showWarnings = F, recursive = T)
  
  cropZone(input_path = input_path,
           output_path = output_path,
           pre_name = tolower(pre_name),
           xmn = xmn,
           xmx = xmx,
           ymn = ymn,
           ymx = ymx)
}
rm(list = ls())
#=============================================================================#
# END OF PROGRAM
#=============================================================================#