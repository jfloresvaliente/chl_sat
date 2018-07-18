# ===============================================================================
# Name   : Loop of Re - grid of Chlorophyll-a
# Author : Hans Jara
# Date   : 03/08/2017
# Version: V1
# Aim    : Loop of data
#===============================================================================
# Link install ------------------------------------------------------------
# library(maps)
library(fields)

dirpath <- '/run/media/marissela/JORGE_NEW/Herve/'
#source("0.Packages.R")
# source("0.Library.R")
#x11()  
# PASO N-1 - Funcion de re-grillado de datos ------------------------------
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

# PASO N-2 - Datos de "LONGITUD" y "LATITUD" ------------------------------
lon_vec = read.csv("Dato_Coordenadas/longitud2.csv", header = F)[,1] #...........................................# estos datos se les vuelve matrix con la finalidad de que tengan las
lat_vec = read.csv("Dato_Coordenadas/latitud2.csv", header = F)[,1] #............................................# mismas dimensiones, se aplica la "transpuesta" (t) debido a que los

numrow <- length(lat_vec)
numcol <- length(lon_vec)

lon <- t(matrix(data = lon_vec, nrow = numrow, ncol = numcol, byrow = T))
lat <- t(matrix(data = lat_vec, nrow = numrow, ncol = numcol, byrow = F))

chl_files <- list.files(path = 'Dato_Clorofila_t/', pattern = 'A2013', full.names = T)
interx <- seq(-100,-70,1/9)
intery <- seq(-40,15,1/9)

new_chl <- array(dim = c(length(interx), length(intery), length(chl_files)))
# for(i in 1:length(chl_files)){
for(i in 1:3){
  data_chl <- t(as.matrix(read.table(file = chl_files[i], header = F, sep = ',')))
  new_chl[,,i] = make.grid2(x = as.vector(lon), y = as.vector(lat), #........................# tama?o de pixel que se quiere obtener
                        z = as.vector(data_chl), interx = interx, intery = intery,
                        fun = function(z) mean (z, na.rm =TRUE))
  print(chl_files[i])
}
chl2D <- apply(X = new_chl, MARGIN = c(1,2), FUN = mean, na.rm = T)

write.table(x = chl2D, file = 'output/meanCHL.csv', row.names = F, col.names = F)
write.table(x = interx, file = 'output/lon_grid.csv', row.names = F, col.names = F)
write.table(x = intery, file = 'output/lat_grid.csv', row.names = F, col.names = F)

par(mfrow = c(1,2))
image.plot(interx, intery, new_chl[,,1], zlim = c(0,10))
map('worldHires', add=T, fill=T, col='gray', ylim = c(-40,15), xlim = c(-100,-70))
image.plot(interx, intery, chl2D, zlim = c(0,10))
map('worldHires', add=T, fill=T, col='gray', ylim = c(-40,15), xlim = c(-100,-70))



















# # PASO N-4 - Bucle de datos Re-Grillado------------------------------------
#   for (a in 1:4739){ #...........................................................................# COMENTARIO N-3: En el bucle ... for (n in 1:N). N, representa la cantidad
#     data_chl1 = read.csv(Dir_chl[a], header = F) #...............................................# de datos, para interx = seq(Glon1,Glon2,P),representa Glon1 y Glon2 son 
#       data_chl2 = as.matrix(data_chl1) #.........................................................# limite max y min de longitud. Para intery = seq(Glat1,Glat2,P), representa
#         data_chl = t(data_chl2) #................................................................# Glon1 y Glon2 son limite max y min de latitud. Para ambos casos "P" es el
#           analisis = make.grid2(x = as.vector(lon), y = as.vector(lat), #........................# tama?o de pixel que se quiere obtener
#           z = as.vector(data_chl), interx = seq(-90,-70,0.1250000037252903),
#           intery = seq(-20,-2,0.1250000037252903), fun = function(z) mean (z, na.rm =TRUE))
#         chl_regrillada[,,a] = analisis }
# 
# # PASO N-5 - Ploteo de los nuevos datos re-grillados ----------------------
#   longitud = c(seq(-90,-70,0.1250000037252903)) #................................................# COMENTARIO N-4: Se generan las nuevas etiquetas de "Longitud" y "Latitud" 
#   latitud = c(seq(-20,-2,0.1250000037252903)) #..................................................# para el ploteo de los datos
#     image.plot(longitud,latitud,chl_regrillada[,,1],zlim=c(0,12),xlab="",ylab="",main="Mapa 2002-07-04",adj=0.5)
#       #map("worldHires",add=T,fill=T,col="white") ; box()
#     
# # PASO N-6 - Generacion de los mapas promedio por semana ------------------
#   chl_promedio = array(NA, dim = c(160,144,677)) #...............................................# COMENTARIO N-5: En el atributo ... chl_regrillada[,,x:y]. x & y, representan
#     for(b in 1:677){ #...........................................................................# los dias que se desean promediar
#       chl_s = apply(chl_regrillada[,,(7*b-6):(7*b)],c(1,2),mean,na.rm=TRUE) 
#       chl_promedio[,,b] = chl_s}
# 
# # PASO N-7 - Bucle para obtener los mapas promediados ---------------------
#   for (e in 1:677){ #............................................................................# COMENTARIO N-6: Como inicialmente se han trabajado con 70 datos, el bucle lee
#     fechas_promedio=seq(from=as.Date("2002-07-04"),to=as.Date("2015-06-18"),by="week") #.........# de 1:10 para obtener promedios semanales. En "fechas_promedio", se emplea un
#       png(file.path("E:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/output_mapas_promedios_12km",paste0(fechas_promedio[e], ".png"))) # etiquetado de YY/MM/DD y el paso es por
#       image.plot(longitud,latitud,chl_promedio[,,e],zlim=c(0,12),ylab="",xlab="",font=2,main=paste0(fechas_promedio[e]," - ","Semana_",e)) # semana. En "png()" se estable la ruta de guardado de los
#         #grid(nx=20, ny=18, col="gray80",lty=1) #................................................# "image.plot()" , y a la vez se le indica los nombres con los que estos ser?n
#         map("worldHires",add=T,fill=T,col="gray89") #............................................# guardados. La funci?n "grid()" es un adicional. El "map()" permite insertar en
#       box(lty=1,lwd=4,col="black") #.............................................................# Inserta un marco con dise?o de marco
#       box(lty=2,lwd=2,col="white") #.............................................................# Inserta un marco con dise?o de marco
#     dev.off()} #.................................................................................# This function closes the specified plot
# 
# # PASO N-8 - Bucle para obtener los mapas diarios -------------------------
#   for (d in 1:4739){ #...........................................................................# COMENTARIO N-7: Los 70 datos incialmente empleados fueron regrillados a una 
#     fechas_dia = seq(from=as.Date("2002-07-04"),to=as.Date("2015-06-24"),by="day") #.............# resoluci?n "R". En "fechas_dia", se emplea un etiquetado de YY/MM/DD y el 
#       png(file.path("E:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/output_mapas_diarios_12km",paste0(fechas_dia[d], ".png"))) # paso es por dia.En "png()" se estable la ruta de 
#         image.plot(longitud,latitud,chl_regrillada[,,d],zlim=c(0,12),main=paste0(fechas_dia[d])) # guardado de los "image.plot()" , y a la vez se le indica los nombres con los que
#         #grid(nx=20, ny=18, col="gray80",lty=1) #................................................# estos ser?n guardados. La funci?n "grid()" es un adicional. El "map()" permite
#         map("worldHires",add=T,fill=T,col="gray89") #............................................# insertar en las im?genes ploteadas una l?nea de costa, la cual puede ser editada.
#       box(lty=1,lwd=4,col="black") #.............................................................# Inserta un marco con dise?o de marco
#       box(lty=2,lwd=2,col="white") #.............................................................# Inserta un marco con dise?o de marco
#     dev.off()} #.................................................................................# This function closes the specified plot
#     
# # # PASO N-9 - Series de tiempo ---------------------------------------------
# # # g 1:160     # h 1:144     # mean 677
# #   for (g in 1:160){
# #   for (h in 1:144){
# #     clo = chl_promedio[g,h,1:677]
# #     if(sum(is.nan(clo)) == length(clo)){
# #       png(file.path("E:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/output_time_series_12km",paste0("X= ",g," - ","Y= ",h,".png")),
# #           width=1440,height=720,units="px",pointsize=12,bg="white",res=NA,family = "",restoreConsole = TRUE)
# #         plot(time,main="sin datos",pch="")
# #           mtext(side=1,"Promedio de datos/Semanas",line=2,cex=1.5,font=2,col="blue")
# #           mtext(side=2,"Concentraci?n de Clorofila",line=2.3,cex=1.5,font=2,col="blue")
# #             dev.off()
# #               next}
# #             time = c(1:677)
# #           png(file.path("E:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/output_time_series_12km",paste0("X= ",g," - ","Y= ",h,".png")),
# #               width=1440,height=720,units="px",pointsize=12,bg="white",res=NA,family = "",restoreConsole = TRUE)
# #         plot(time,clo,type="o",pch=20,cex=1.5,cex.axis=1.2,lty=1,lwd=1,col="black",font=2,ylab="",xlab="",main=paste0("X= ",g," - ","Y= ",h))
# #       mtext(side=1,"Promedio de datos/Semanas",line=2,cex=1.5,font=2,col="blue")
# #       mtext(side=2,"Concentraci?n de Clorofila",line=2.3,cex=1.5,font=2,col="blue")
# #     dev.off() 
# #   } }
#     
# # PASO N-10 - Spline ------------------------------------------------------
#   for (g in 1:160){
#   for (h in 1:144){
#     clo = chl_promedio[g,h,1:677]
#       if(sum(is.nan(clo)) == length(clo)){
#         png(file.path("E:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/output_time_series_12km",paste0("X= ",g," - ","Y= ",h,".png")),
#             width=1440,height=720,units="px",pointsize=12,bg="white",res=NA,family = "",restoreConsole = TRUE)
#           plot(time,main="sin datos",pch="")
#             mtext(side=1,"Promedio de datos/Semanas",line=2,cex=1.5,font=2,col="blue")
#             mtext(side=2,"Concentraci?n de Clorofila",line=2.3,cex=1.5,font=2,col="blue")
#               dev.off()
#                 next}
#               time = c(1:677)
#             png(file.path("E:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/output_time_series_12km",paste0("X= ",g," - ","Y= ",h,".png")),
#                 width=1440,height=720,units="px",pointsize=12,bg="white",res=NA,family = "",restoreConsole = TRUE)
#           plot(time,clo,type="o",pch=20,cex=1.5,cex.axis=1.2,lty=1,lwd=1,col="black",font=2,ylab="",xlab="",main=paste0("X= ",g," - ","Y= ",h))
#         points(spline(time,clo,n=677,xout=seq(1,677,1),method="natural"),pch=20,cex=1.5,col="red")
#         lines(spline(time,clo,n=677,xout=seq(1,677,1),method="natural"),lty=3,lwd=1,col="red")
#       mtext(side=1,"Promedio de datos/Semanas",line=2,cex=1.5,font=2,col="blue")
#       mtext(side=2,"Concentraci?n de Clorofila",line=2.3,cex=1.5,font=2,col="blue")
#     dev.off()
#   } }
# 
# # PASO N-11 - CSV ---------------------------------------------------------
#   for (g in 1:160){
#   for (h in 1:144){
#     clo = chl_promedio[g,h,1:677]
#       if(sum(is.nan(clo)) == length(clo)){
#         int_spline = spline(time,n=677,xout=seq(1,677,1),method="natural")
#           write.csv(int_spline,file.path("E:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/output_csv",paste0("X= ",g," - ","Y= ",h,".csv")),row.names = FALSE)
#             next}
#             time = c(1:677)
#           int_spline = spline(time,clo,xout=seq(1,677,1),method="natural")
#         write.csv(int_spline,file.path("E:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/output_csv",paste0("X= ",g," - ","Y= ",h,".csv")),row.names = FALSE)
#   } }
# 
# ###########################################################################
# # SOLO PARA QUE RECUERDE --------------------------------------------------
# ###########################################################################
# 
# # Los datos de "CLOROFILA" estaban invertidos por lo que se opto por aplicarle la funci?n "transpuesta (t)"
# # Como esto se aplico a los datos de CHL, se aplica del mismo modo a los datos de LONGITUD y LATITUD 
# 
# # Si usted cambia el valor de "P" en el PASO N-4, inter""=seq(G1,G2,P), debera cambiar las dimensiones A y B
# # presentes en el PASO N-3, chl_interpolada = array(NA, dim = c(A,B,N))
# 
# # Por ejemplo, si usted quiere cambiar el tama?o de resolucion (R) a obtener, solo debe cambiar (R) en "interx" e "intery",
# # RECUERDE QUE: 
# # (1) el rango (Lon y Lat) es frente a la costa peruana.
# # (2) Para correr este ejemplo primero debe correr los PASOS N-1, 2 y 3.
# # Las dimensiones obtenidas de reemplazan desde el PASO N-3, PASO N-6 y PASO N-9.
# # La nueva resoluci?n se reemplaza en los PASOS N-4 y 5.
# 
# setwd("J:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/Dato_Clorofila")
# Dir_chl = dir() 
# n=1
# data_chl1 = read.csv(Dir_chl[n], header = F)
# data_chl2 = as.matrix(data_chl1)
# data_chl = t(data_chl2)
# analisis_ejemplo = make.grid2(x = as.vector(lon), y = as.vector(lat), z = as.vector(data_chl), 
#                               interx = seq(-90,-70,0.1250000037252903),intery = seq(-20,-2,0.1250000037252903), fun = function(z) mean (z, na.rm =TRUE))
# image.plot(analisis_ejemplo)
# grid(lty = 1)
# map("worldHires",add=T,fill=T,col="lightgray")
# dim(analisis_ejemplo)
# 
# # RECUERDA:
# Abreviatura        Resoluci?n (?)          Resoluci?n (km)   Dimensiones [x , y]
# Grilla original     Go          R = 0.0416666679084301          ~ 4            X = 480 ; Y = 432
#                     G1          R = 0.1250000037252903          ~ 12           X = 160 ; Y = 144
#                     G2          R = 0.2083333395421505          ~ 20           X =  96 ; Y =  87
#                     G3          R = 0.52083334885537625         ~ 50           X =  39 ; Y =  35
#                     G4          R = 1.04166669777107525         ~ 100          X =  20 ; Y =  18
# 
# 
# clo1 = chl_promedio[4,4,1:12]
#   plot(time,clo1,type="o",pch=19,lty=1,lwd=1,col="black",font=2,ylab="",xlab="",main=paste0("X= ",4," - ","Y= ",4))
#   points(spline(time,clo1,n=12,method="natural"),pch=21,lwd=1,col="red")
#   #lines(spline(time,clo,n=12,method="natural"),lwd=1,col="red")
#     mtext(side=1,"Promedio de datos/Semanas",line=2,cex=1,font=2,col="blue")
#     mtext(side=2,"Concentraci?n de Clorofila",line=2.3,cex=1,font=2,col="blue")
# 
# clo1 = chl_promedio[4,4,1:12] 
# int_spline1 = spline(time,clo1,n=12,method="natural")
# 
# setwd("J:/IMARPE - Files/Analisis_Datos/Analisis_Clorofila/output_csv") ; dir()
# write.csv(int_spline1, file = "dato1.csv",row.names = FALSE)
