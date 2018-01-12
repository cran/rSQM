
GetQmapFit2 <- function(obsdata, rcpdata, varnm, monid) {

  #vars = c("prcp", "tmax", "tmin", "wspd", "rhum", "rsds")
  ovarnm <- paste("obs_",varnm,sep="")
  rvarnm <- varnm

  obsmon <- obsdata[which(obsdata[,2]==monid),which(colnames(obsdata)==ovarnm)]
  obsmon <- as.numeric(obsmon[!is.na(obsmon)])
  rcpmon <- rcpdata[which(rcpdata[,2]==monid),which(colnames(rcpdata)==rvarnm)]

  if(varnm == "prcp"){
    qmf <- qmap::fitQmap(obsmon, rcpmon, method="QUANT", qstep=0.01, wet.day=T, na.rm=T)
  } else {
    qmf <- qmap::fitQmap(obsmon, rcpmon, method="QUANT", qstep=0.01, wet.day=F, na.rm=T)
  }
  
  ## Logic for avoding unique values
  tmp <- qmf$par$modq
  if(length(unique(tmp))==1) tmp[1:4] <- c(0.1, 0.075, 0.05, 0.025)
  qmf$par$modq <- tmp
  
  return(qmf)
}
