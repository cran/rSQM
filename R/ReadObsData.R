ReadObsData <- function(stnid, wdir) {


  srchstr <- paste("*", stnid, "*.csv", sep="")
  obsfile <- list.files(wdir, pattern = glob2rx(srchstr), full.names=F)
  obs <- read.csv(file.path(wdir, obsfile), header=T, na.strings = c("-99.00", "-99.0", "-99", -99, "NA", NA))

  datestr <- paste(obs[[1]],obs[[2]],obs[[3]],sep="-")
  date <- as.Date(datestr,"%Y-%m-%d")
  obs <- cbind(date, obs[,c(4:9)])

  year <- as.numeric (format(obs[,1],"%Y"))
  mon <- as.numeric (format(obs[,1],"%m"))
  day <- as.numeric (format(obs[,1],"%d"))
  yearmon <- as.character (format(obs[,1],"%Y-%m"))

  data <- cbind(year, mon, day, yearmon, obs)

  return(data)
}
