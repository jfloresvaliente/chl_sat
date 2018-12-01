dat <- read.table('C:/Users/ASUS/Desktop/Chla_interp_areas.csv', sep = ';', header = T)

get_num_picks <- function(serie){
  picks <- rle(serie)
  picks <- cbind(picks$lengths, picks$values)
  picks <- subset(picks, picks[,2] == 1)
  picks <- dim(picks)[1]
  return(picks)
}

umbrales <- seq(0,30, 0.5)
zonas_picks <- NULL
for(i in umbrales){
  umbral <- i
  NUMpicks <- t(dat)
  NUMpicks[NUMpicks <  umbral] <- 0
  NUMpicks[NUMpicks >= umbral] <- 1
  picks_number <- apply(NUMpicks, c(1), get_num_picks)
  picks_number <- c(picks_number, i)
  zonas_picks <- rbind(zonas_picks, picks_number)
}

colnames(zonas_picks) <- c(colnames(dat), 'umbral')
