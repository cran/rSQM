#' @title Update Observation Data from GHCN(Global Historical Climatology Network)
#'
#' @description When user does not have own observation dataset, one can download it from GHCN. However, not recommended since NA values are too many.
#'
#' @param NtlCode 2 digit country(national) code, EnvList$NtlCode
#' @param stndir directory where downloaded data from GHCN located, EnvList$stndir
#' @param syear_obs start year of observation, EnvList$syear_obs
#' @param eyear_obs end year of observation, EnvList$eyear_obs
#'
#' @examples 
#' 
#' \dontrun{
#' ## You can download observation data of Myanmar(MY) from 1969 to 2005 into current working directory
#' GhcnDailyUpdate(NtlCode="MY", stndir=getwd(), syear_obs=1969, eyear_obs=2005)
#' }
#' @return NULL
#' @export
#'
GhcnDailyUpdate <- function(NtlCode, stndir, syear_obs, eyear_obs) {

  options(warn=-1)
  options(stringsAsFactors = FALSE)

  #tmp <- getwd()

  DownDir <- file.path(stndir, "download")
  if(!file.exists(DownDir)) dir.create(DownDir, showWarnings=F,recursive=T)

  stnfile <- file.path(DownDir, "ghcnd-stations.txt")
  ivntfile <- file.path(DownDir, "ghcnd-inventory.txt")
  cntryfile <- file.path(DownDir, "ghcnd-countries.txt")


  stn_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
  download.file(stn_url, stnfile, mode = "wb")

  ivnt_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"
  download.file(ivnt_url, ivntfile, mode = "wb")

  cnt_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-countries.txt"
  download.file(cnt_url, cntryfile, mode = "wb")

  stndata <- read.fwf(stnfile, header = FALSE, widths = c(11, 10, 10, 7, 80),
                     col.names = c("ID", "Lat", "Lon", "Elev", "Ename"))

  # The National Codes used in ADSS and GHCN are different sometimes.
  NtlCodes_Diff <- matrix(c("BD","BG", # col 1st : ADSS national codes, col 2nd : GHCN national codes
                            "CA","CA",
                            "CL","CI",
                            "CU","CU",
                            "EG","EG",
                            "ET","ET",
                            "FM","FM",
                            "ID","ID",
                            "IN","IN",
                            "KE","KE",
                            "KR","KS", # South Korea... (ADSS Code should be checked later.)
                            "MH","RM",
                            "MM","BM",
                            "MN","MG",
                            "IR","IR",
                            "MY","MY",
                            "NP","NP",
                            "PH","RP",
                            "PK","PK",
                            "TH","TH",
                            "TO","TN",
                            "TZ","TZ",
                            "VN","VM",
                            "WS","AQ",
                            "ZM","ZA"), nrow = 25, ncol = 2, byrow = TRUE, dimnames = list(NULL,c("ADSS", "GHCN")))
  ivntdata <- read.table(ivntfile, header = F, col.names = c("ID", "Lat", "Lon", "var", "syear", "eyear"))
  if(nchar(NtlCode)==4 && substr(NtlCode,1,2)=="US")
  {
    # User want US`s State-wise data
    srchstr <- paste("^", substr(NtlCode,3,4), sep ="")
#    stndata <- stndata %>% filter(stringr::str_detect(Ename, srchstr))
    stndata <- stndata[stringr::str_detect(stndata[,"Ename"], srchstr),]
    stndata[which(stndata$Lon < 0), "Lon"] <- stndata[which(stndata$Lon < 0), "Lon"] + 360
    Station_Names <- stndata$ID; stncnt <- length(Station_Names)
    CntryCode <- NtlCode
  } else {
    # Not US
    CntryCode <- NtlCodes_Diff[(NtlCodes_Diff[,1]==NtlCode),2]
    srchstr <- paste("^", CntryCode, sep ="")
#    stndata <- stndata %>% filter(stringr::str_detect(ID, srchstr))
    stndata <- stndata[stringr::str_detect(stndata[,"ID"], srchstr),]
    stndata[which(stndata$Lon < 0), "Lon"] <- stndata[which(stndata$Lon < 0), "Lon"] + 360
    Station_Names <- stndata$ID; stncnt <- length(Station_Names)
  }


  flag <- TRUE
  for(i in 1:stncnt)
    {
    Station_Name <- Station_Names[i]
    ivnt <- ivntdata[which(ivntdata[,"ID"] == Station_Name & (ivntdata[,"var"] == "PRCP" | ivntdata[,"var"] == "TMAX" | ivntdata[,"var"] == "TMIN")), ]
    if(nrow(ivnt) > 0)
      {
      syear <- max(ivnt[,"syear"])
      eyear <- min(ivnt[,"eyear"])
      if(syear <= syear_obs & eyear >= eyear_obs)
        {
        stninfo <- stndata[which(stndata$ID == Station_Name), c("Lon", "Lat", "Elev", "ID", "Ename")]
        stninfo$SYear <- syear
        cat("Load data from station :", stninfo$ID,"\n")
        url <- paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/", Station_Name, ".dly", sep = "")
        download.file(url, file.path(DownDir, paste(Station_Name, ".dly", sep = "")) , mode = "wb")

        dlyfile <- paste(Station_Name, ".dly", sep="")
        csvfile <- paste(Station_Name, ".csv", sep="")
        ConvertGhcnDaily(DownDir, stndir, dlyfile, csvfile)

        if(flag)
        {
          stnout <- stninfo
          flag <- FALSE
        } else {
          imsi <- stninfo
          stnout <- rbind(stnout, imsi)
        }
        cat("Station",stninfo[,"ID"],"data successfully loaded.\n\n")
      } # end of if(syear <= syear_obs & eyear >= eyear_obs)
    } # end of if(nrow(ivnt)>0)
  } # end of for(i in 1:stncnt)

  if(flag)
  {
    # There is no available Ghcn Observation Data
    stop("No Available Observations at GHCN.\n
          Recommended solution : 1. Run Again. For some reason, the meta files 'ghcnd-stations.txt' and 'ghcnd-inventory.txt'\n
                                    are downloaded incompletely from time to time, which makes this process go wrong. \n
                                 2. If above trial does not work, this implies that GHCN has no appropriate observation data.\n
                                    Try with your own observation data.\n")
  }else{
    stnoutfile <- file.path(stndir, "Station-Info.csv")
    write.csv(stnout, stnoutfile, row.names=F)
  }
  unlink(DownDir, recursive = TRUE)
} # end of GhcnDailyUpdate
