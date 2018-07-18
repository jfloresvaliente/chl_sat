# ===============================================================================
# Name   : 
# Author : 
# Date   : 
# Version: 
# Aim    : 
#===============================================================================
dirpath <- 'D:/Herve/'
make.grid2 = function(x = x, y = y, z = z, interx = interx, intery = intery, fun){
  X=interx
  Y=intery
  byx2=c(diff(interx),diff(interx)[length(diff(interx))])
  byy2=c(diff(intery),diff(interx)[length(diff(interx))])
  i <- which(x >= min(interx) - 0.5 * byx2[1] & x < max(interx) + 0.5 *
               byx2[length(byx2)] & y >= min(intery) - 0.5 * byy2[1] & y 
             < max(intery) + 0.5 * byy2[length(byy2)])
  xi <- findInterval(x[i], X - 0.5 * byx2)
  yi <- findInterval(y[i], Y - 0.5 * byy2)
  grid0 <- tapply(z[i], list(X[xi], Y[yi]), fun)
  Xi <- match(X, rownames(grid0))
  Yi <- match(Y, colnames(grid0))
  grid1 <- grid0[Xi, Yi]
  dimnames(grid1) <- list(X, Y)
  return(grid1)
}

lon_vec = read.csv(paste0(dirpath,"Dato_Coordenadas/longitud2.csv"), header = F)[,1] #...........................................# estos datos se les vuelve matrix con la finalidad de que tengan las
lat_vec = read.csv(paste0(dirpath,"Dato_Coordenadas/latitud2.csv"), header = F)[,1] #............................................# mismas dimensiones, se aplica la "transpuesta" (t) debido a que los

numrow <- length(lat_vec)
numcol <- length(lon_vec)

lon <- t(matrix(data = lon_vec, nrow = numrow, ncol = numcol, byrow = T))
lat <- t(matrix(data = lat_vec, nrow = numrow, ncol = numcol, byrow = F))

chl_files <- list.files(path = paste0(dirpath,'Dato_Clorofila_t/'), pattern = 'A2011', full.names = F)
interx <- seq(-100,-70,1/9)
intery <- seq(-40,15,1/9)

for(i in 1:length(chl_files)){
  data_chl <- t(as.matrix(read.table(file = paste0(dirpath,'Dato_Clorofila_t/',chl_files[i]), header = F, sep = ',')))
  new_chl  <- make.grid2(x = as.vector(lon), y = as.vector(lat),
                        z = as.vector(data_chl), interx = interx, intery = intery,
                        fun = function(z) mean (z, na.rm =TRUE))
  write.table(x = new_chl, file = paste0(dirpath,'output/regrid/',chl_files[i]), row.names = F, col.names = F)
  print(chl_files[i])
}
xgrid <- matrix(data = interx, nrow = length(interx), ncol = length(intery))
ygrid <- matrix(data = intery, nrow = length(interx), ncol = length(intery), byrow = T)
write.table(x = xgrid, file = paste0(dirpath,'output/regrid/lon_grid.txt'), row.names = F, col.names = F)
write.table(x = ygrid, file = paste0(dirpath,'output/regrid/lat_grid.txt'), row.names = F, col.names = F)
#===============================================================================
# END OF PROGRAM
#===============================================================================