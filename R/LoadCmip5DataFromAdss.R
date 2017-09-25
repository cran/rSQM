#' @title Download clipped national level CMIP5 data
#'
#' @description Download clipped national level CMIP5 data from ADSS`s ftp server. Apec Climate Center Data Service System
#'
#' @param dbdir directory where downloaded data located
#' @param NtlCode 2 digit country(national) code
#'
#' @examples
#' \dontrun{
#' rSQMSampleProject() 
#' SetWorkingEnvironment(envfile = "rSQM.yaml")
#' LoadCmip5DataFromAdss(dbdir = EnvList$dbdir, NtlCode = EnvList$NtlCode)
#' ## You can find the national code at www.apcc21.org 
#' }
#' @return NULL
#' @export
#'
LoadCmip5DataFromAdss <- function(dbdir, NtlCode) {

  fname <- paste("cmip5_daily_", NtlCode, ".zip", sep="")

  if(nchar(NtlCode)==4 && substr(NtlCode,1,2)=="US"){
    adss <- "ftp://cis.apcc21.org/CMIP5DB/US/"
  }else{
    adss <- "ftp://cis.apcc21.org/CMIP5DB/"
  }

  srcfname <- paste(adss, fname, sep="")
  dstfname <- paste(dbdir, "/", fname, sep = "")
  download.file(srcfname, dstfname, mode = "wb")
  unzip(dstfname, exdir = dbdir)
  unlink(dstfname, force = T)
  cat("CMIP5 scenario data at",NtlCode,"is successfully loaded.\n")
}

