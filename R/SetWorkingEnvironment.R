#' @title Setting project environment with external yaml file
#'
#' @description User needs quite many arguments throughtout the process. For accomodation, one can write down yaml file with environmental arguments and by using it, everythig is done.
#'
#' @param envfile  yaml file including environmental infomation
#' @param override override
#' 
#' @examples
#' 
#' rSQMSampleProject() 
#' SetWorkingEnvironment(envfile = "rSQM.yaml")
#' 
#' \dontrun{
#' EnvList <- SetWorkingEnvironment(envfile = "UserCustomized.yaml")
#' ## And this EnvList object has all the information for the all the next steps
#' }
#' 
#' @return a list object containing all environmental arguments
#' 
#' @export
SetWorkingEnvironment  <- function(envfile, override=list()) {


  data <- yaml::yaml.load_file(envfile)

  data <- lapply(data, function(x) if (is.character(x)) gsubfn::gsubfn("\\$\\((.*?)\\)", data, x) else x)

  cmip5dir <- paste(data$dbdir, "/cmip5_daily_", data$NtlCode, sep="")

  if(data$stndir == "User") data$stndir <- data$stnobsdir
  if(data$stndir == "GHCN") data$stndir <- data$ghcndir

  if(!file.exists(data$qmapdir)) dir.create(data$qmapdir, showWarnings=F,recursive=T)
  if(!file.exists(data$stndir)) dir.create(data$stndir, showWarnings=F,recursive=T)
  if(!file.exists(data$bnddir)) dir.create(data$bnddir, showWarnings=F,recursive=T)
#  if(!file.exists(data$ghcndir)) dir.create(data$ghcndir, showWarnings=F,recursive=T)

  outList <- list("prjdir" = data$prjdir,
                  "dbdir" = data$dbdir,
                  "qmapdir" = data$qmapdir,
                  "bnddir" = data$bnddir,
                  "stndir" = data$stndir,
#                  "ghcndir" = data$ghcndir,
                  "syear_obs" = data$syear_obs,
                  "eyear_obs"= data$eyear_obs,
                  "syear_his" = data$syear_his,
                  "eyear_his" = data$eyear_his,
                  "syear_scn" = data$syear_scn,
                  "eyear_scn" = data$eyear_scn,
                  "SimAll" = data$SimAll,
                  "ModelNames" = data$ModelNames,
                  "RcpNames" = data$RcpNames,
                  "VarNames" = data$VarNames,
                  "NtlCode" = data$NtlCode,
                  "stndir" = data$stndir,
                  "stnfile" = data$stnfile,
                  "bndfile" = data$bndfile,
                  "OWrite" = data$OWrite,
                  "SRadiation" = data$SRadiation,
                  "cmip5dir" = cmip5dir)
  # override
  for (varname in names(override)) {
    outList[[varname]] <- override[[varname]]
  }
  return(outList)

}

