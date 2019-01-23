library(raster)
library(maps)
library(mapdata)

fechas <- read.table('D:/Clorofila/dates_carbajal.csv', header = F, sep = ',')
dirpath <- 'D:/Clorofila/crop_Cherrepe/'

for(i in 1:dim(fechas)[1]){
  
  mifecha  <- fechas[i,]
  year_in  <- mifecha[,1]
  month_in <- mifecha[,2]
  day_in   <- mifecha[,3]

      serieday_in <- seq(from = as.Date(paste0(year_in, '-', 1, '-', 01)),
                         to = as.Date(paste0(year_in, '-', 12, '-', 31)), by = 'day')
      
      myday_in <- as.Date(paste0(year_in, '-', month_in, '-', day_in))
      PNG1 <- paste0(dirpath, myday_in, '.png')
      
      day_in_index <- which(serieday_in == myday_in)
      
      chl_files <- list.files(path = paste0(dirpath, year_in), full.names = T)
      chl <- chl_files[day_in_index]
      chl <- raster(chl)
      
      png(filename = PNG1, width = 850, height = 850, res = 120)
      plot(chl, axes = F, main = paste0(year_in, '-',month_in, '-', day_in))
      map('worldHires', add=T, fill=T, col='gray')#, xlim = c(xmn, xmx), ylim = c(ymn, ymx))
      grid()
      axis(side = 1, font = 2, lwd.ticks = 2, cex.axis = 1.5)
      axis(side = 2, font = 2, lwd.ticks = 2, cex.axis = 1.5, las = 2)
      box(lwd = 2)
      dev.off()
}


