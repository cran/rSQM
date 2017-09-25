GetQmapFit <- function(mrgdata, varnm, monid) {

  ovarnm = paste("obs_",varnm,sep="")
  rvarnm = varnm

  obsmon = mrgdata[which(mrgdata[, "mon"]==monid),which(colnames(mrgdata)==ovarnm)]
  obsmon = as.numeric(obsmon[!is.na(obsmon)])
  rcpmon = mrgdata[which(mrgdata[, "mon"]==monid),which(colnames(mrgdata)==rvarnm)]

  if(varnm == "prcp"){
    qmf = qmap::fitQmap(obsmon,rcpmon, method="QUANT", qstep=0.01, wet.day=T, na.rm=T)
  } else {
    qmf = qmap::fitQmap(obsmon,rcpmon, method="QUANT", qstep=0.01, wet.day=F, na.rm=T)
  }

  return(qmf)
}
