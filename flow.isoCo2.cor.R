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
  #Para$Flow$PrdIncrPf <- 1
  Para$Flow$PrdWndwCalc <- 7
  #Para$Flow$PrdWndwPf <- 1
  Para$Flow$Read <- c("hdf5", "ff")[1]
  Para$Flow$VersDp <- NA
  Para$Flow$VersEddy <- NA
  
}

# INITIALIZE ENVIRONMENT #######################################################

# define global environment
eddy4R.base::def.env.glob()

logLevel <- Sys.getenv('LOG_LEVEL')
if (is.null(logLevel) || logLevel == "") {
  assign("rlog", NEONprocIS.base::def.log.init(Lvl="debug"), envir = .GlobalEnv)
} else {
  assign("rlog", NEONprocIS.base::def.log.init(), envir = .GlobalEnv)
}
rlog$info("Start ECSE CO2 isotope correction from flow.isoCo2.cor.R")


# check if require packages are installed 
packReq <- c("NEONiso", "parsedate", "Hmisc", "neonUtilities", "dplyr")

lapply(packReq, function(x) {
  tryCatch({rlog$debug(x)}, error=function(cond){print(x)})
  if(require(x, character.only = TRUE) == FALSE) {
    install.packages(x)
    library(x, character.only = TRUE)
  }})
# remove workflow package list
rm(packReq)
# load and attach packages

#install packages from Cran 
#install.packages("neonUtilities", dependencies=TRUE, repos='http://cran.rstudio.com/')
#neonUtilities >= 2.2.1
#install.packages("NEONiso", dependencies=TRUE, repos='http://cran.rstudio.com/')
#NEONiso >= 0.6.1
#install.packages("rlang", dependencies=TRUE, repos='http://cran.rstudio.com/')
#‘rlang’ >= 1.1.0 is required

# names of packages
namePack <- c("DataCombine", "eddy4R.base", "eddy4R.turb", "NEONiso", "neonUtilities", "methods", "rhdf5", "deming", "dplyr") 


# load and attach
tmp <- sapply(namePack, library, character.only = TRUE); rm(tmp)


# INITIAL CHECKS ###############################################################

# check if number of input files (Para$Flow$FileInp) cover calculation window (Para$Flow$PrdWndwCalc)
# i.e. if Para$Flow$PrdWndwCalc is equal to 7, Para$Flow$FileInp should be equal to 7 files
if(Para$Flow$PrdWndwCalc > base::length(Para$Flow$FileInp)) {
  msg <- "number of daily input files < Para$Flow$PrdWndwCalc."
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}

# check that Para$Flow$DateOut is a strictly regular, daily sequence
if(!base::all(base::diff(base::as.Date(Para$Flow$DateOut)) == 1)) {
  msg <- "please ensure that Para$Flow$DateOut consists of subsequent days."
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}

# determine center days
dateCntr <- Para$Flow$DateOut[base::seq(from = 1, to = base::length(Para$Flow$DateOut), by = Para$Flow$PrdIncrCalc)]

# for each center day, determine the date sequence covering its planar-fit window
for(idx in dateCntr) {
  
  Para$Flow$DateCalc[[idx]] <- base::as.character(base::seq.Date(
    from = base::as.Date(idx) - ((base::as.integer(Para$Flow$PrdWndwCalc) - 1) / 2),
    to = base::as.Date(idx) + ((base::as.integer(Para$Flow$PrdWndwCalc) - 1) / 2),
    by = "day"))
  
}; rm(idx)

# check if there is an input file for each day over all calculation windows
# determine if there is an input file corresponding to each day
dateCalcAll <- sapply(base::sort(base::unique(base::unlist(Para$Flow$DateCalc))), function(x)
  base::any(base::grepl(pattern = base::paste0(".*", x, ".*.h5?"), Para$Flow$FileInp))
)

# issue an error in case one or more files are missing
if(!base::all(dateCalcAll)) {
  msg <- base::paste("data for", base::paste0(base::names(dateCalcAll)[!dateCalcAll], collapse = ", "), "is missing.")
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}

# Processing ###################################################################
# determine full input file path for current center day in data processing window
# this day is used as reference for reading parametric information
DirFilePara <- base::file.path(Para$Flow$DirInp, grep(pattern = base::paste0(".*", dateCntr, ".*.h5?"), Para$Flow$FileInp, value = TRUE))

# Grab the NEON specific 4-letter code for the site location (Loc) from the dp0p input file
Para$Flow$Loc <- eddy4R.base::def.para.site(FileInp = DirFilePara)$Loc

#working directory path
#dir <- paste0("~/eddy/data/iso/",site)
#input data directory path
#inpDir <- paste0(dir,"/inp")

#list all file names in inpDir
nameFile <- list.files(path = Para$Flow$DirInp,
                       pattern = '.h5',
                       recursive = TRUE,
                       full.names = TRUE)
#Output file names (center day)
nameOutFileTmp <- gsub(".h5",".calibrated.h5",DirFilePara)
nameOutFileSplt <- strsplit(nameOutFileTmp, split = "/")
#get output file names
nameOutFileOut <- sapply(nameOutFileSplt, '[[', length(nameOutFileSplt[[1]]))

#output data directory path
outDir <- Para$Flow$DirOut
#output file names with directory
nameOutFileOut <- paste0(outDir,"/",nameOutFileOut)

#correction processing
#correcting data using Bowling_2003 method
#outData01 <- outData
outData01 <- calibrate_carbon(nameFile,nameOutFileOut,site=Para$Flow$Loc, method = "Bowling_2003", write_to_file = FALSE)

#correcting data using Linear regression (linreg) method
#NOTE: values of min, max output from linreg method are the corrected values not raw values
#outData02 <- outData
outData02 <- calibrate_carbon(nameFile,nameOutFileOut,site=Para$Flow$Loc, method = "linreg", write_to_file = FALSE)
#combine data from both method into one list
outData <- list()
#combine cal_df
outData$cal_df <- cbind(outData01$cal_df, outData02$cal_df[,-which(names(outData02$cal_df) %in% c("timeBgn", "timeEnd"))])

#adding columns, combine results, clean up, organizing, and changing column names
for(j in names(outData01$ciso_subset_cal)) {
  for (k in names (outData01$ciso_subset_cal[[j]])){
  if (k %in% c("dlta13CCo2", "rtioMoleDryCo2")){
    #change column names to NEON terms
    #original column names of outData01 (Bowling_2003 method)
    #dlta13CCo2 = c("timeBgn","timeEnd","mean","min","max","vari","numSamp","mean_cal","min_cal","max_cal","CVcalUcrt","LOOcalUcrt")
    #rtioMoleDryCo2 = c("timeBgn","timeEnd","mean","min","max","vari","numSamp","mean_cal","CVcalUcrt","LOOcalUcrt")
    #colnames(outData01$ciso_subset_cal[[j]][[k]]) <- c("timeBgn","timeEnd","mean","min","max","vari","numSamp","meanCorBowl","minCorBowl","maxCorBowl","cvCalUcrt","looCalUcrt")
    
    #original column names of outData02 (linreg method)
    #c("timeBgn","timeEnd","mean","min","max","vari","numSamp","mean_cal","cvloo", "cv5rmse", "cv5mae")
    #NOTE: values of min, max output from linreg method are the corrected values not raw values
    #replace column names of min & max to minCor and maxCor
    #names(outData02$ciso_subset_cal[[j]][[k]]) <- c("timeBgn","timeEnd","mean","minCorLinReg","maxLinReg","vari","numSamp","meanCorLinReg","cvLoo","cv5Rmse","cv5Mae")
    
    #create temporary table to combine results from both methods in NEON format
    tmpData <- data.frame(
      #provided best estimated (output from Bowling_2003 method)
      mean = outData01$ciso_subset_cal[[j]][[k]]$mean_cal,
      min = outData01$ciso_subset_cal[[j]][[k]]$min_cal,
      max = outData01$ciso_subset_cal[[j]][[k]]$max_cal,
      vari = outData01$ciso_subset_cal[[j]][[k]]$vari, #vari not calculate after applying calibration (should talk to Rich)
      numSamp = outData01$ciso_subset_cal[[j]][[k]]$numSamp,
      timeBgn = outData01$ciso_subset_cal[[j]][[k]]$timeBgn,
      timeEnd = outData01$ciso_subset_cal[[j]][[k]]$timeEnd,
      #output from Bowling_2003 method
      meanCorBowl = outData01$ciso_subset_cal[[j]][[k]]$mean_cal,
      minCorBowl = outData01$ciso_subset_cal[[j]][[k]]$min_cal,
      maxCorBowl = outData01$ciso_subset_cal[[j]][[k]]$max_cal,
      cvCalUcrt =outData01$ciso_subset_cal[[j]][[k]]$CVcalUcrt,
      looCalUcrt =outData01$ciso_subset_cal[[j]][[k]]$LOOcalUcrt,
      #output from linreg
      meanCorLinReg = outData02$ciso_subset_cal[[j]][[k]]$mean_cal,
      minCorLinReg =  outData02$ciso_subset_cal[[j]][[k]]$min,#not calculate after applying calibration (should talk to Rich)
      maxCorLinReg =  outData02$ciso_subset_cal[[j]][[k]]$max,#not calculate after applying calibration (should talk to Rich)
      cvLoo = outData02$ciso_subset_cal[[j]][[k]]$cvloo,
      cv5Rmse = outData02$ciso_subset_cal[[j]][[k]]$cv5rmse,
      cv5Mae = outData02$ciso_subset_cal[[j]][[k]]$cv5mae,
      #raw/non corrected data
      meanRaw = outData01$ciso_subset_cal[[j]][[k]]$mean,
      minRaw = outData01$ciso_subset_cal[[j]][[k]]$min,
      maxRaw = outData01$ciso_subset_cal[[j]][[k]]$max,
      variRaw = outData01$ciso_subset_cal[[j]][[k]]$vari
    )
    #outData$ciso_subset_cal[[j]][[k]] <-  cbind(outData01$ciso_subset_cal[[j]][[k]], 
     #                                           outData02$ciso_subset_cal[[j]][[k]][,-which(names(outData02$ciso_subset_cal[[j]][[k]]) %in% c("timeBgn", "timeEnd", "mean", "vari", "numSamp"))])
    outData$ciso_subset_cal[[j]][[k]] <- tmpData
  }
    else{outData$ciso_subset_cal[[j]][[k]] <- outData01$ciso_subset_cal[[j]][[k]]}
    }
  names(outData$ciso_subset_cal[[j]]) <- names(outData01$ciso_subset_cal[[j]])
}

# # import data into workspace
# # create list to hold data
# data <- base::list()
# for(idx in base::names(dateCalcAll)) {
#   # idx <- base::names(dateCalcAll)[1]
#   
#   if(length(Para$Flow$FileInp[idx]) == 1){
#     
#     rlog$debug(base::paste("begin: reading", Para$Flow$FileInp[[idx]]))
#     data[[idx]] <- eddy4R.base::def.hdf5.extr(FileInp = base::file.path(Para$Flow$DirInp, grep(pattern = base::paste0(".*", idx, ".*.h5?"), Para$Flow$FileInp, value = TRUE)))
#     rlog$debug(base::paste("complete: reading", Para$Flow$FileInp[[idx]]))
#     
#   } else {stop(base::paste("file missing for", idx), ".")}
# }
# 
# #extract data from input hdf5 files
# tmp <- eddy4R.base::def.hdf5.extr(FileInp = DirFilePara,
#                                   FileOut = base::paste0(Para$Flow$DirOut,"/",Para$Flow$FileOutBase,".calibrated.h5"))



################################################
##TEST####
inname <- nameFile
outname <- nameOutFileOut
site <- Para$Flow$Loc
method <- c("Bowling_2003", "linreg")[2]
calibration_half_width <- 0.5
force_cal_to_beginning <- TRUE
force_cal_to_end <- TRUE
gap_fill_parameters <- FALSE
filter_ambient <- TRUE
r2_thres <- 0.95
correct_refData <- TRUE
write_to_file <- FALSE
remove_known_bad_months <- TRUE
plot_regression_data <- FALSE
plot_directory <- NULL



# if there are more than 1 file in inname, merge all files together after running ingest_data()

if (length(inname) > 1) {
  ciso <- list()

for (i in 1:length(inname)){
  #i <- 1
  tmp <- NEONiso:::ingest_data(inname[i], analyte = 'Co2')
  if(i == 1){
    ciso <- tmp
    #ciso1 <- ciso
  } else {
    #append all ingest ambient data 
    for(j in names(tmp$ambient)) {
      ciso$ambient[[j]] <- lapply(names(tmp$ambient[[j]]), function(y){
        rbind(ciso$ambient[[j]][[y]], tmp$ambient[[j]][[y]])
      })
      names(ciso$ambient[[j]]) <- names(tmp$ambient[[j]])
      }#End of for loop around levels
     
    #append all ingest reference data 
    for(k in names(tmp$reference)) {
      ciso$reference[[k]] <- lapply(names(tmp$reference[[k]]), function(y){
        rbind(ciso$reference[[k]][[y]], tmp$reference[[k]][[y]])
      })
      names(ciso$reference[[k]]) <- names(tmp$reference[[k]])
    }#End of for loop around levels
    #append all refe_stacked 
    ciso$refe_stacked <- rbind(ciso$refe_stacked, tmp$refe_stacked)
  }
  
}}

# extract the data we need from ciso list
refe <- NEONiso:::extract_carbon_calibration_data(ciso$refe_stacked)



