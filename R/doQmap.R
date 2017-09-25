DoQmap <- function(qmf, rcpdata, varnm, monid, sdate, edate) {

  sdate <- as.Date(sdate)
  edate <- as.Date(edate)

  qmf <- get(qmf)

  rcpprd <- rcpdata[which(rcpdata[,2]==monid & rcpdata[,4]>=sdate & rcpdata[,4]<=edate),which(colnames(rcpdata)==varnm)]

  date <- rcpdata[which(rcpdata[,2]==monid & rcpdata[,4]>=sdate & rcpdata[,4]<=edate), 4]

  rcpadj <- qmap::doQmap(rcpprd, qmf)
  rcpadj <- cbind.data.frame(date, rcpadj)
  colnames(rcpadj) <- c("date", varnm)

  rcpadj[,2] <- rcpadj[,2]

  return(rcpadj)
}
