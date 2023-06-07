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
  Para$Flow$FileOutBase <- c("ECSE_dp04_ONAQ_2017-09-17")
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
#install NEONiso from GitHub. Development version
#devtools::install_github("lanl/NEONiso")
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

# for each center day, determine the date sequence covering calculate window (i.e. 7)
for(idx in dateCntr) {
  
  Para$Flow$DateCalc[[idx]] <- base::as.character(base::seq.Date(
    from = base::as.Date(idx) - ((base::as.integer(Para$Flow$PrdWndwCalc) - 1) / 2),
    to = base::as.Date(idx) + ((base::as.integer(Para$Flow$PrdWndwCalc) - 1) / 2),
    by = "day"))
  
}; rm(idx)

# check if there is an input file (expanded file) for each day over all calculation windows
# determine if there is an input file corresponding to each day
dateCalcExpd <- sapply(base::sort(base::unique(base::unlist(Para$Flow$DateCalc))), function(x)
  base::any(base::grepl(pattern = base::paste0(".*", x, ".expanded.h5?"), Para$Flow$FileInp))
)

# issue an error in case one or more files are missing
if(!base::all(dateCalcExpd)) {
  msg <- base::paste("expanded file for", base::paste0(base::names(dateCalcExpd)[!dateCalcExpd], collapse = ", "), "is missing.")
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}

# check if there is an input file (basic file) for the center day (this file will be used for writing output)
# determine if there is an input file corresponding to center day

dateCalcBasc <- sapply(base::sort(base::unique(base::unlist(Para$Flow$DateCalc))), function(x)
  base::any(base::grepl(pattern = base::paste0(".*", x, ".basic.h5?"), Para$Flow$FileInp))
)

# issue an error in case basic file center day are missing
if(!dateCalcBasc[[dateCntr]]) {
  msg <- base::paste("basic file for ", dateCntr, " is missing.")
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}
# Processing ###################################################################
# determine full input file path for current center day in data processing window
# this day is used as reference for reading parametric information
# Expanded file path
Para$FileName$EcseExpd <- base::file.path(Para$Flow$DirInp, grep(pattern = paste0("ECSE.*",Para$Flow$DateOut ,".*expanded.h5"), Para$Flow$FileInp, value = TRUE))
#DirFilePara <- base::file.path(Para$Flow$DirInp, grep(pattern = base::paste0(".*", dateCntr, ".*.h5?"), Para$Flow$FileInp, value = TRUE))

# Basic file path
Para$FileName$EcseBasc <- base::file.path(Para$Flow$DirInp, grep(pattern = paste0("ECSE.*",Para$Flow$DateOut ,".*basic.h5"), Para$Flow$FileInp, value = TRUE))

# Grab the NEON specific 4-letter code for the site location (Loc) from the dp0p input file
Para$Flow$Loc <- eddy4R.base::def.para.site(FileInp = Para$FileName$EcseExpd)$Loc

#working directory path
#dir <- paste0("~/eddy/data/iso/",site)
#input data directory path
#inpDir <- paste0(dir,"/inp")

#In this case, expanded files will be used in calculation
#list all file names in inpDir
nameFile <- list.files(path = Para$Flow$DirInp,
                       pattern = "expanded.h5",
                       recursive = TRUE,
                       full.names = TRUE)

# This part can be delete ####################################################
#Output file names (center day)
#nameOutFileTmp <- gsub(".h5",".calibrated.h5",DirFilePara)
#nameOutFileSplt <- strsplit(nameOutFileTmp, split = "/")
#get output file names
#nameOutFileOut <- sapply(nameOutFileSplt, '[[', length(nameOutFileSplt[[1]]))

#output file names with directory
#nameOutFileOut <- paste0(Para$Flow$DirOut,"/",nameOutFileOut)
# DELETE #####################################################################

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

#adding columns, combine results, clean up, organizing, and changing column names
for(j in names(outData01$ciso_subset_cal)) {
  for (k in names (outData01$ciso_subset_cal[[j]])){
  if (k %in% c("dlta13CCo2", "rtioMoleDryCo2")){
    
    ######BOOK KEEPING######################################
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
    #######################################################
    
    #adding min_cal and max_cal to rtioMoleDryCo2
    if (k %in% c("rtioMoleDryCo2")) {
      outData01$ciso_subset_cal[[j]][[k]]$min_cal <- NaN
      outData01$ciso_subset_cal[[j]][[k]]$max_cal <- NaN
    }
    #create temporary table to combine results from both methods in NEON format
    tmpData <- data.frame(
      #provided best estimated (output from Bowling_2003 method)
      mean = outData01$ciso_subset_cal[[j]][[k]]$mean_cal,
      min = outData01$ciso_subset_cal[[j]][[k]]$min_cal,
      max = outData01$ciso_subset_cal[[j]][[k]]$max_cal,
      vari = NaN, #vari not calculate after applying calibration (should talk to Rich)
      numSamp = outData01$ciso_subset_cal[[j]][[k]]$numSamp,
      #output from Bowling_2003 method
      meanCorBowl = outData01$ciso_subset_cal[[j]][[k]]$mean_cal,
      minCorBowl = outData01$ciso_subset_cal[[j]][[k]]$min_cal,
      maxCorBowl = outData01$ciso_subset_cal[[j]][[k]]$max_cal,
      cvCalUcrt =outData01$ciso_subset_cal[[j]][[k]]$CVcalUcrt,
      looCalUcrt =outData01$ciso_subset_cal[[j]][[k]]$LOOcalUcrt,
      #output from linreg
      meanCorLinReg = outData02$ciso_subset_cal[[j]][[k]]$mean_cal,
      minCorLinReg =  outData02$ciso_subset_cal[[j]][[k]]$min, #min, max output from linreg method are the corrected values not raw values
      maxCorLinReg =  outData02$ciso_subset_cal[[j]][[k]]$max,
      cvLoo = outData02$ciso_subset_cal[[j]][[k]]$cvloo,
      cv5Rmse = outData02$ciso_subset_cal[[j]][[k]]$cv5rmse,
      cv5Mae = outData02$ciso_subset_cal[[j]][[k]]$cv5mae,
      #raw/non corrected data
      meanRaw = outData01$ciso_subset_cal[[j]][[k]]$mean,
      minRaw = outData01$ciso_subset_cal[[j]][[k]]$min,
      maxRaw = outData01$ciso_subset_cal[[j]][[k]]$max,
      variRaw = outData01$ciso_subset_cal[[j]][[k]]$vari,
      timeBgn = outData01$ciso_subset_cal[[j]][[k]]$timeBgn,
      timeEnd = outData01$ciso_subset_cal[[j]][[k]]$timeEnd
    )
    #replace NA value with NaN
    tmpData[is.na(tmpData)] <- NaN
    #outData$ciso_subset_cal[[j]][[k]] <-  cbind(outData01$ciso_subset_cal[[j]][[k]], 
     #                                           outData02$ciso_subset_cal[[j]][[k]][,-which(names(outData02$ciso_subset_cal[[j]][[k]]) %in% c("timeBgn", "timeEnd", "mean", "vari", "numSamp"))])
    outData$ciso_subset_cal[[j]][[k]] <- tmpData
  }
    else{
      #rearrange timeBgn & timeEnd to last column (NEON format)
      outData$ciso_subset_cal[[j]][[k]] <- cbind(outData01$ciso_subset_cal[[j]][[k]][,-which(names(outData01$ciso_subset_cal[[j]][[k]]) %in% c("timeBgn", "timeEnd"))],
                                                 timeBgn = outData01$ciso_subset_cal[[j]][[k]]$timeBgn, 
                                                 timeEnd = outData01$ciso_subset_cal[[j]][[k]]$timeEnd)
        }#end else
    }
  names(outData$ciso_subset_cal[[j]]) <- names(outData01$ciso_subset_cal[[j]])
}

#create a new cal_df table for each calibration method
outData$cal_df_Bowl <- cbind(outData01$cal_df[,-which(names(outData01$cal_df) %in% c("timeBgn", "timeEnd"))],
                             timeBgn = outData01$cal_df$timeBgn, 
                             timeEnd = outData01$cal_df$timeEnd)
outData$cal_df_Linreg <- cbind(outData02$cal_df[,-which(names(outData02$cal_df) %in% c("timeBgn", "timeEnd"))],
                               timeBgn = outData01$cal_df$timeBgn, 
                               timeEnd = outData01$cal_df$timeEnd)#there was an issue of time output from this method

#outData$cal_df <- cbind(outData01$cal_df[,-which(names(outData01$cal_df) %in% c("timeBgn", "timeEnd"))],
#                        outData02$cal_df[,-which(names(outData02$cal_df) %in% c("timeBgn", "timeEnd"))],
#                        timeBgn = outData01$cal_df$timeBgn, 
#                        timeEnd = outData01$cal_df$timeEnd)

#change column names to NEON terms
colnames(outData$cal_df_Bowl) <- c("slp12C", "ofst12C", "rsq12C", "cvLoo12C", "cv5Mae12C", "cv5Rmse12C",
                              "slp13C", "ofst13C", "rsq13C", "cvLoo13C", "cv5Mae13C", "cv5Rmse13C",
                              "timeBgn", "timeEnd")
                              
colnames(outData$cal_df_Linreg) <- c("slpDlta13CCo2", "ofstDlta13CCo2", "rsqDlta13CCo2", "cvLooDlta13CCo2", "cv5MaeDlta13CCo2", "cv5RmseDlta13CCo2",
                              "slpRtioMoleDryCo2", "ofstRtioMoleDryCo2", "rsqRtioMoleDryCo2", "cvLooRtioMoleDryCo2", "cv5MaeRtioMoleDryCo2", "cv5RmseRtioMoleDryCo2",
                              "timeBgn", "timeEnd")

#grab center day data
dataDateCntr <- outData
#get start and end time
timeBgn <- base::as.POSIXct(paste(dateCntr, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
timeEnd   <- base::as.POSIXct(paste(dateCntr, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#convert NEON time to POSIXct
for (i in c("cal_df_Bowl", "cal_df_Linreg")){
dataDateCntr[[i]]$timeBgn <- as.POSIXct(paste(dataDateCntr[[i]]$timeBgn, " ", "00:00:00.00", sep=""), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", origin = "1970-01-01 00:00:00")
dataDateCntr[[i]]$timeBgn <- base::as.POSIXct(paste(dataDateCntr[[i]]$timeBgn, " ", "00:00:00.001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
dataDateCntr[[i]]$timeEnd <- as.POSIXct(dataDateCntr[[i]]$timeEnd, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", origin = "1970-01-01 00:00:00")
#subset center day data for cal_df table
dataDateCntr[[i]] <- dataDateCntr[[i]][which(dataDateCntr[[i]]$timeEnd >= timeBgn & dataDateCntr[[i]]$timeBgn <= timeEnd),]
#convert time back to NEON hdf5 time
dataDateCntr[[i]]$timeBgn <- NEONiso:::convert_POSIXct_to_NEONhdf5_time(dataDateCntr[[i]]$timeBgn)
dataDateCntr[[i]]$timeEnd <- NEONiso:::convert_POSIXct_to_NEONhdf5_time(dataDateCntr[[i]]$timeEnd)
}#end loop i


#subset center day data for ciso_subset_cal
for(j in names(dataDateCntr$ciso_subset_cal)) {
  for (k in names (dataDateCntr$ciso_subset_cal[[j]])){
    #convert NEON time to POSIXct
    dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn <- as.POSIXct(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", origin = "1970-01-01 00:00:00")
    dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd <- as.POSIXct(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", origin = "1970-01-01 00:00:00")
    #subset data
    dataDateCntr$ciso_subset_cal[[j]][[k]] <-dataDateCntr$ciso_subset_cal[[j]][[k]][which(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd >= timeBgn & dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn <= timeEnd),]
    #convert time back to NEON hdf5 time
    dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn <- NEONiso:::convert_POSIXct_to_NEONhdf5_time(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn)
      dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd <-  NEONiso:::convert_POSIXct_to_NEONhdf5_time(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd) 
    }#end loop k
}#end loop j


#Writing data to HDF5 ############
#create list to hold data
data <- list()
#extract data from expanded hdf5 file for the center day
data$Expd <- eddy4R.base::def.hdf5.extr(FileInp = Para$FileName$EcseExpd)
#extract data from basic hdf5 file for the center day
data$Basc <- eddy4R.base::def.hdf5.extr(FileInp = Para$FileName$EcseBasc)

#replace isoCo2 calibrate data
for(j in names(dataDateCntr$ciso_subset_cal)) {
  for (k in c("dlta13CCo2", "rtioMoleDryCo2")){
    var <- paste0("/",Para$Flow$Loc, "/dp01/data/isoCo2","/",j,"/",k)
  data$Expd$listData[[var]] <- dataDateCntr$ciso_subset_cal[[j]][[k]]
  data$Basc$listData[[var]] <- dataDateCntr$ciso_subset_cal[[j]][[k]]
  #report only mean, min, max, vari, and numSamp (results from Bowling method in this case) for basic file
  data$Basc$listData[[var]] <- dataDateCntr$ciso_subset_cal[[j]][[k]][,which(names(dataDateCntr$ciso_subset_cal[[j]][[k]]) %in% c("mean", "min", "max", "vari","numSamp","timeBgn", "timeEnd"))]
  
  #unit re-assignment (only expanded files)
  if(k %in% c("dlta13CCo2")){
    #mean,min,max,vari,numSamp,
    #meanCorBowl,minCorBowl,maxCorBowl,cvCalUcrt,looCalUcrt,
    #meanCorLinReg,minCorLinReg,maxCorLinReg,cvLoo,cv5Rmse,cv5Mae,
    #meanRaw,minRaw,maxRaw,variRaw,timeBgn,timeEnd
    data$Expd$listAttr[[var]]$unit <-c("permill","permill","permill","permill2","NA",
                                       "permill","permill","permill", "permill2", "permill2",
                                       "permill","permill","permill", "permill2", "permill2","permill2",
                                       "permill","permill","permill","permill2","NA","NA")
  } else {
    data$Expd$listAttr[[var]]$unit <-c("umolCo2 mol-1","umolCo2 mol-1","umolCo2 mol-1","umol2Co2 mol-2","NA",
                                       "umolCo2 mol-1","umolCo2 mol-1","umolCo2 mol-1", "umol2Co2 mol-2", "umol2Co2 mol-2",
                                       "umolCo2 mol-1","umolCo2 mol-1","umolCo2 mol-1", "umol2Co2 mol-2", "umol2Co2 mol-2","umol2Co2 mol-2",
                                       "umolCo2 mol-1","umolCo2 mol-1","umolCo2 mol-1","umol2Co2 mol-2","NA","NA") 
  }

  }#end k loop
}#end j loop

#write out to hdf5
tmp <- eddy4R.base::def.hdf5.extr(FileInp = NULL,
                                  rpt = data$Expd,
                                  FileOut = base::paste0(Para$Flow$DirOut,"/",Para$Flow$FileOutBase, "expanded.h5"))
tmp <- eddy4R.base::def.hdf5.extr(FileInp = NULL,
                                  rpt = data$Basc,
                                  FileOut = base::paste0(Para$Flow$DirOut,"/",Para$Flow$FileOutBase, "basic.h5"))

rm(tmp)

#writing calibration parameters
print("Writing calibration parameters...")

for (packIdx in c("expanded.h5", "basic.h5")){
outname <- base::paste0(Para$Flow$DirOut,"/",Para$Flow$FileOutBase, packIdx)  
rhdf5::h5createGroup(outname,base::paste0("/",Para$Flow$Loc, "/dp01/data/isoCo2/calData"))

fid <- rhdf5::H5Fopen(outname)
calLoc <- rhdf5::H5Gopen(fid, paste0("/", Para$Flow$Loc, "/dp01/data/isoCo2/calData"))

if (packIdx %in% c("expanded.h5")) {
  #writing calibration parameter for Bowling method
  rhdf5::h5writeDataset(obj = dataDateCntr$cal_df_Bowl,
                        h5loc = calLoc,
                        name = "calDataBowl",
                        DataFrameAsCompound = TRUE)
  #writing units
  #slp12C, ofst12C, rsq12C, cvLoo12C, cv5Mae12C, cv5Rmse12C,
  #slp13C, ofst13C, rsq13C, cvLoo13C, cv5Mae13C, cv5Rmse13C,
  #timeBgn", "timeEnd"
  unitBowl <- c("NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA")

  #open calDataBowl dataframe
  calLocBowl <- rhdf5::H5Dopen(calLoc, "calDataBowl")
  rhdf5::h5writeAttribute(unitBowl, h5obj = calLocBowl, name = "unit")
  
  #writing calibration parameter for LinReg method
  rhdf5::h5writeDataset(obj = dataDateCntr$cal_df_Linreg,
                        h5loc = calLoc,
                        name = "calDataLinReg",
                        DataFrameAsCompound = TRUE)
  #writing units
  #slpDlta13CCo2, ofstDlta13CCo2, rsqDlta13CCo2, cvLooDlta13CCo2, cv5MaeDlta13CCo2, cv5RmseDlta13CCo2,
  #slpRtioMoleDryCo2, ofstRtioMoleDryCo2, rsqRtioMoleDryCo2, cvLooRtioMoleDryCo2, cv5MaeRtioMoleDryCo2, cv5RmseRtioMoleDryCo2,
  #timeBgn, timeEnd
  unitLinReg <- c("NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA")
  
  #open calDataLinReg dataframe
  calLocLinReg <- rhdf5::H5Dopen(calLoc, "calDataLinReg")
  rhdf5::h5writeAttribute(unitLinReg, h5obj = calLocLinReg, name = "unit")
  
} else if (packIdx %in% c("basic.h5")) {
  rhdf5::h5writeDataset(obj = dataDateCntr$cal_df_Bowl,
                        h5loc = calLoc,
                        name = "calDataBowl",
                        DataFrameAsCompound = TRUE)
  #writing units
  #slp12C, ofst12C, rsq12C, cvLoo12C, cv5Mae12C, cv5Rmse12C,
  #slp13C, ofst13C, rsq13C, cvLoo13C, cv5Mae13C, cv5Rmse13C,
  #timeBgn", "timeEnd"
  unitBowl <- c("NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA")
  
  #open calDataBowl dataframe
  calLocBowl <- rhdf5::H5Dopen(calLoc, "calDataBowl")
  rhdf5::h5writeAttribute(unitBowl, h5obj = calLocBowl, name = "unit")
}

rhdf5::H5Gclose(calLoc)

# close the group and the file
rhdf5::H5Fclose(fid)
rhdf5::h5closeAll()
}#end loop packIdx

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
#need to source extract_carbon_calibration_data() so the results will be match with 0.6.1 version
#source('~/eddy/docker/NEONiso-NDurden/R/reference_data_extraction.R')

inname <- nameFile
outname <- NULL
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



