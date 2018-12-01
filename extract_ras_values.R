library(raster)
library(fields)
library(maps)
library(mapdata)
library(imputeTS)

dirpath <- 'D:/Clorofila/crop_Sechura/'

rasterFiles <- list.files(path = dirpath, pattern = '.*\\.nc', full.names = T, recursive = T)
ras <- raster(rasterFiles[1]); dim(ras)

rasArray <- array(dim = c(dim(ras)[1] * dim(ras)[2], length(rasterFiles)) ); dim(rasArray)
for(i in 1:length(rasterFiles)){
  rasArray[,i] <- getValues(raster(rasterFiles[i]))
  print(rasterFiles[i])
}
# dim(rasArray)

dfna <- numeric(length = dim(rasArray)[1])
for(i in 1:dim(rasArray)[1]){
  dfna[i] <- sum(!is.na(rasArray[i,]))
}
ras[] <- dfna
x11()
plot(ras)
map('worldHires', add=T, fill=T, col='gray')





# df <- apply(rasArray, c(1), mean, na.rm = T); length(df)
# ras[] <- df
# x11()
# plot(ras)
# map('worldHires', add=T, fill=T, col='gray')



# # Con esto se puede bambiar la resolucion del raster
# library(raster)
# x <- raster(matrix(1:24, 4)); dim(x)
# y <- aggregate(x, fact = 2, fun = mean, na.rm = TRUE); dim(y)

# dim(x); dim(y)
# res(x); res(y)
