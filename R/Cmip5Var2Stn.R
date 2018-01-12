Cmip5Var2Stn <- function(stnid, varnms, rcpnm, wdir, mdlnm) {

#  setwd(wdir)
  nvar = length(varnms)
  #rcpname = c("historical", "rcp26", "rcp45", "rcp60", "rcp85")
  for (i in 1:nvar) {

    # User have to define RCP scenario : rcp[i]
    #rcpfile = paste(mdlnm,"_",rcpname[(rcpid+1)],"_",varnms[i],".csv",sep="")
    rcpfile <- file.path(wdir, paste(mdlnm,"_",rcpnm,"_",varnms[i],".csv",sep=""))
    ##LOG
    cat("rcp file : ", rcpfile, "\n")
    if(i==1) {

      if(file.exists(rcpfile)) {
        rcp_var1 <- read.csv(rcpfile, header=T, na.strings = -99.00)
        coln <- match(stnid, names(rcp_var1))
        nrows <- length(rcp_var1[,1])
        rcpstn <- rcp_var1[ ,c(1,coln)]
        colnames(rcpstn) <- c("date", varnms[i])
        rcpstn$date <- as.Date(rcpstn$date)
      } else {
        sdate <- as.Date("1976-01-01")
        edate <- as.Date("2100-12-31")
        dummydate <- seq(sdate, edate, by=1)
        nrow <- length(dummydate)
        dummyval <- rep(-99.00, times=nrow)
        rcpstn <- as.data.frame(cbind(dummydate, dummyval))
        colnames(rcpstn) <- c("date", varnms[i])
        rcpstn$date <- as.Date(dummydate)

      } # end of IF

    } else {

      if(file.exists(rcpfile)) {
        rcp_imsi <- read.csv(rcpfile,header = TRUE, na.strings = -99.00)
        coln <- match(stnid, names(rcp_imsi))
        nrows <- length(rcp_imsi[,1])
        rcp_imsi <- rcp_imsi[ ,c(1,coln)]
        colnames(rcp_imsi) <- c("date", varnms[i])
        rcp_imsi$date <- as.Date(rcp_imsi$date)
        rcpstn <- merge(rcpstn, rcp_imsi, by="date", all=T)
      } else {
        sdate <- as.Date("1976-01-01")
        edate <- as.Date("2100-12-31")
        dummydate <- seq(sdate, edate, by=1)
        nrow <- length(dummydate)
        dummyval <- rep(-99.00, times=nrow)
        rcp_imsi <- as.data.frame(cbind(dummydate, dummyval))
        colnames(rcp_imsi) <- c("date", varnms[i])
        rcp_imsi$date <- as.Date(dummydate)
        rcpstn <- merge(rcpstn, rcp_imsi, by="date", all=T)
      } # end of IF
    } # end of IF
  } # varialbe LOOP

  rcpstn[is.na(rcpstn)] <- -99.00

  data <- FillDate(rcpstn)

  return(data)
}
