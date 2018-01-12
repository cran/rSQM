GetQmapFit <- function(mrgdata, varnm, monid) {

  ovarnm <- paste("obs_",varnm,sep="")
  rvarnm <- varnm

  obsmon <- mrgdata[which(mrgdata[, "mon"]==monid),which(colnames(mrgdata)==ovarnm)]
  obsmon <- as.numeric(obsmon[!is.na(obsmon)])
  rcpmon <- mrgdata[which(mrgdata[, "mon"]==monid),which(colnames(mrgdata)==rvarnm)]

  if(varnm == "prcp"){
    qmf <- qmap::fitQmap(obsmon,rcpmon, method="QUANT", qstep=0.01, wet.day=TRUE, na.rm=TRUE)
  } else {
    qmf <- qmap::fitQmap(obsmon,rcpmon, method="QUANT", qstep=0.01, wet.day=FALSE, na.rm=TRUE)
  }
  
  
  ## Logic for avoding unique values
  tmp <- qmf$par$modq
  if(length(unique(tmp))==1) tmp[1:4] <- c(0.1, 0.075, 0.05, 0.025)
  qmf$par$modq <- tmp
  
  return(qmf)
}
