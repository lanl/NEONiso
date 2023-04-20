##############################################################################################
#' @title Workflow for calibrating NEON atmospheric CO2 isotope data

#' @author
#' Natchaya Pingintha-Durden \email{eddy4R.info@gmail.com} \cr
#' 

#' @description
#' Workflow. Calibrating NEON atmospheric CO2 isotope data as described in Fiorella et al. (2021).

#' @param Currently none

#' @return Currently none

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr
#' Metzger, S., Durden, D., Sturtevant, C., Luo, H., Pingintha-Durden, N., Sachs, T., Serafimovich, A., Hartmann, J., Li, J., Xu, K., and Desai, A. R.: eddy4R: A community-extensible processing, analysis and modeling framework for eddy-covariance data based on R, Git, Docker and HDF5, Geosci. Model Dev. Discuss., 2017, 1-26, doi:10.5194/gmd-2016-318.
#' Fiorella, R. P., Good, S. P., Allen, S. T., Guo, J. S., Still, C. J., Noone, D. C., Anderegg, W. R. L.,  Florian, C. R., Luo, H., Pingintha-Durden, N., and Bowen, G.J.: Calibration Strategies for Detecting Macroscale Patterns in NEON Atmospheric Carbon Isotope Observations, J. Geophys. Res. Biogeosci., 2021, 126(3), e2020JG005862, https://doi.org/10.1029/2020JG005862.

#' @keywords carbon, isotope, storage, eddy-covariance, calibration

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   Natchaya Pingintha-Durden (2023-04-18)
#     original creation

##############################################################################################
# USER SELECTIONS


# deploy workflow file in default mode (dflt, uses web-based gold file), in environmental-variables-from-host mode (host, for batch-processing from command line),
# or in user selection mode (slct, for interactive data analysis in Rstudio) [character]
# see bottom of this section for command line instruction templates

# create list to hold parametric information in a central place
Para <- list()

# user selection in case corresponding environmenal variable is not assigned
if(!"METH" %in% base::names(base::Sys.getenv())) {
  Para$Flow$Meth <- c("dflt",  "host", "slct")[3]
  # if corresponding environmenal variable is assigned, use that value
} else {
  Para$Flow$Meth <- base::Sys.getenv("METH")
}


# in case user selection mode is chosen (Para$Flow$Meth == "slct"), workflow parameters can be modified
if(Para$Flow$Meth == "slct") {
  
  # user-customizeable access to input and output data directories
  Para$Flow$DirUsr <- c(
    nd = "/eddy/data/iso/ONAQ",
    dd = "/eddy/data/tmp"
  )["nd"]
  # for a detailed description of all default workflow parameters see ?eddy4R.base::def.para.flow.ecte, section Overview of workflow parameters
  Para$Flow$DateOut <- base::as.character(base::as.Date(base::as.character(20170917:20170917), format = "%Y%m%d"))[1:1]
  Para$Flow$DirInp <- base::paste0("/home/", base::Sys.getenv("USER"), Para$Flow$DirUsr, "/inp")
  Para$Flow$DirMnt <- base::paste0("/home/", Sys.getenv("USER"), "/eddy")
  Para$Flow$DirOut <- base::paste0("/home/", Sys.getenv("USER"), Para$Flow$DirUsr, "/out")
  Para$Flow$DirTmp <- base::paste0("/home/", Sys.getenv("USER"), "/eddy/tmp")
  Para$Flow$DirWrk <- NA
  Para$Flow$FileInp <- base::dir(Para$Flow$DirInp, pattern = "*.h5")
  Para$Flow$FileOutBase <- c("ECSE_dp04_ONAQ_2017-09-17.expanded", "ECSE_dp04_ONAQ_2017-09-17.basic")[1]
  Para$Flow$NameDataExt <- NA
  Para$Flow$OutMeth <- c("hdf5", "diag")[1]
  Para$Flow$OutSub <- NA
  Para$Flow$PrdIncrCalc <- 1
  Para$Flow$PrdIncrPf <- 1
  Para$Flow$PrdWndwCalc <- 7
  Para$Flow$PrdWndwPf <- NA
  Para$Flow$Read <- c("hdf5", "ff")[1]
  Para$Flow$VersDp <- NA
  Para$Flow$VersEddy <- NA
  
}

# INITIALIZE ENVIRONMENT

# define global environment
eddy4R.base::def.env.glob()

logLevel <- Sys.getenv('LOG_LEVEL')
if (is.null(logLevel) || logLevel == "") {
  assign("rlog", NEONprocIS.base::def.log.init(Lvl="debug"), envir = .GlobalEnv)
} else {
  assign("rlog", NEONprocIS.base::def.log.init(), envir = .GlobalEnv)
}
rlog$info("Start ECSE CO2 isotope correction from flow.isoCo2.cor.R")


#check if require packages are installed 
packReq <- c("NEONiso", "parsedate", "Hmisc", "neonUtilities")

lapply(packReq, function(x) {
  tryCatch({rlog$debug(x)}, error=function(cond){print(x)})
  if(require(x, character.only = TRUE) == FALSE) {
    install.packages(x)
    library(x, character.only = TRUE)
  }})
#Remove workflow package list
rm(packReq)
# load and attach packages

# names of packages
namePack <- c("DataCombine", "eddy4R.base", "eddy4R.turb", "NEONiso", "neonUtilities", "methods", "rhdf5", "deming") 


# load and attach
tmp <- sapply(namePack, library, character.only = TRUE); rm(tmp)

# initial checks

# check if number of processing files (Para$Flow$FileInp) cover calculation window (Para$Flow$PrdWndwCalc)
# i.e. if Para$Flow$PrdWndwCalc is equal to 7, Para$Flow$FileInp should be equal to 7 files
if(Para$Flow$PrdWndwCalc > base::length(Para$Flow$FileInp)) {
  msg <- "number of daily input files < Para$Flow$PrdWndwCalc."
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}

#NEON site
site <- "ONAQ"

#working directory path
dir <- paste0("~/eddy/data/tmp/",site)
#input data directory path
inpDir <- paste0(dir,"/inp")
  
#list all file names in inpDir
nameFile <- list.files(path = inpDir,
                       pattern = '.h5',
                       recursive = TRUE,
                       full.names = TRUE)
#Output file names
nameOutFileTmp <- gsub(".h5",".calibrated.h5",nameFile)
nameOutFileSplt <- strsplit(nameOutFileTmp, split = "/")
#get output file names
nameOutFileOut <- sapply(nameOutFileSplt, '[[', length(nameOutFileSplt[[1]]))

#output data directory path
outDir <- paste0(dir,"/out")
#output file names
nameOutFileOut <- paste0(outDir,"/",nameOutFileOut)

#correction processing
for (i in 1:length(nameOutFileOut)) {
  calibrate_carbon_bymonth(nameFile[i],nameOutFileOut[i],site=site, method = "Bowling_2003")
}

