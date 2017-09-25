#' @title function to create simple environment for package tutorial
#' 
#' @description  Creates project directories in user's current working directory and load sample project data into proper path for user to perform simple tutorial. 
#'
#' @param verbose Logical. If false, you can skip the verbose description for the structure
#' 
#' @examples
#' library(rSQM)
#' rSQMSampleProject()
#' ## Check your working directory, you can see newly created directories and sample data being loaded.
#' @return NULL
#' @export
rSQMSampleProject <- function(verbose=TRUE) # Function Name Should be modified.
{
  if(verbose){ mise::mise(vars = FALSE, figs = FALSE) } 
  prjDir <- "rSQM_Sample_Project"; SetWorkingDir(prjDir)
  dbDir <- "Database"; SetWorkingDir(dbDir)
  SetWorkingDir(file.path(prjDir,"Observed"))
  stndir <- file.path(prjDir ,"Observed" ,"SampleProject"); SetWorkingDir(stndir)

  spl_env <- system.file("extdata", "rSQM.yaml", package = "rSQM")
#  print(spl_env)
  spl_stn_info <- system.file("extdata", "Sample_3-Stations.csv", package = "rSQM")
  spl_stn_dat1 <- system.file("extdata", "Sample_ID108.csv", package = "rSQM")
  spl_stn_dat2 <- system.file("extdata", "Sample_ID133.csv", package = "rSQM")
  spl_stn_dat3 <- system.file("extdata", "Sample_ID159.csv", package = "rSQM")
  spl_script <- system.file("extdata", "sample_script.R", package = "rSQM")

  ## Sample Data Location Description
  if(!file.copy(spl_env, "./", overwrite = TRUE))
  { stop("\tAn error occurs when sample data loaded, reinstall package and try again.\n\tIf does not work again, contact <pablito@apcc21.org>, the maintainer.") }
  if(!file.copy(spl_stn_info, stndir, overwrite = TRUE))
  { stop("\tAn error occurs when sample data loaded, reinstall package and try again.\n\tIf does not work again, contact <pablito@apcc21.org>, the maintainer.") }
  if(!file.copy(spl_stn_dat1, stndir, overwrite = TRUE))
  { stop("\tAn error occurs when sample data loaded, reinstall package and try again.\n\tIf does not work again, contact <pablito@apcc21.org>, the maintainer.") }
  if(!file.copy(spl_stn_dat2, stndir, overwrite = TRUE))
  { stop("\tAn error occurs when sample data loaded, reinstall package and try again.\n\tIf does not work again, contact <pablito@apcc21.org>, the maintainer.") }
  if(!file.copy(spl_stn_dat3, stndir, overwrite = TRUE))
  { stop("\tAn error occurs when sample data loaded, reinstall package and try again.\n\tIf does not work again, contact <pablito@apcc21.org>, the maintainer.") }
  if(!file.copy(spl_script, "./", overwrite = TRUE))
  { stop("\tAn error occurs when sample data loaded, reinstall package and try again.\n\tIf does not work again, contact <pablito@apcc21.org>, the maintainer.") }
  
  SetWorkingEnvironment(envfile="rSQM.yaml")

  if(verbose) {
    aa <- function(){
      cat("\n")
      cat("\t APEC Climate Center. www.apcc21.org \n\n")
      cat("\t Statistical Downscaling Package 'rSQM'\n")
      cat("\t Environmental Setting for Sample Project.\n\n")
      deco()
      cat("\n\t Environment Settings Done.\n\n")
    }
    aa()
  
    bb <- function(){
      cat("*************************Directory Structure********************************\n\n")
      cat("\t\t\t\t*-------Downscale/SQM\n")
      cat("\t\t\t\t|\t(qmapdir)\n")
      cat("\t*-------rSQM_Sample_Project\n")
      cat("\t|\t(prjdir)\t|\n")
      cat("\t|\t\t\t*-------Observed/Sample\n")
      cat("\t|\t\t\t|\t(stndir)\n")
      cat("\t|\t\t\t|\n")
      cat("\t|\t\t\t*-------gis-boundary\n")
      cat("\t|\t\t\t\t(bnddir)\n")
      cat("\t|\t\t\t\n")
      cat("working directory\n")
      cat("\t|\n")
      cat("\t|\n")
      cat("\t*-------Database\n")
      cat("\t\t(dbdir)\n")
      cat("\n")
      cat("*************************Directory Description*****************************\n\n")
      cat("dbdir : The database directory to store daily-CMIP5 data, gis-boundary data,\n\t")
      cat("\tobservation data, and station information data\n\n")
      cat("stndir : The directory to store station infomation andclimatological observation data.\n")
      cat("\t There are two ways to prepare observation data\n")
      cat("\t 1. If you have your own custom observation data, put them in this\n")
      cat("\t    directory.\n")
      cat("\t 2. Otherwise, You can use GHCN, Global Historical Climatory Network, \n")
      cat("\t    which provides global climatological observations, such as TMAX/TMIN\n")
      cat("\t    and PRCP etc. They are nationally clipped and served as csv files.\n\n")
      cat("prjdir : The project directory to store results. For each downscaling method,\n")
      cat("\t corresponding subdirectory is created and contains the results.\n\n")
    }
    bb()

    cc <- function(){
      deco()
      cat("\n")
      cat("*************************Sample Station and Observation Data Loaded********************************\n")
      cat("\tSample station infomation and obseravation data are loaded in 'stndir'.\n")
      cat("\tand the R script(Sample_script.R) to run the sample project with sample data\n")
      cat("\t is located in your current working directory. Have a look and enjoy it!\n\n")
      cat("\t APEC Climate Center. www.apcc21.org \n")
    }
    cc()
  }
  

} # End of RunSampleProject()

