#' Title
#'
#' @param data_path Provide path to where calibrated data from one site live.
#' @param plot_path 
#' @param method Bowling or linear regression calibration method? (Valid values: Bowling, LinReg)
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @importFrom magrittr %>%
#' 
carbon_diagnostic_package <- function(data_path,
                                      plot_path,
                                      method,
                                      which.sites="all") {
  
  # what plots would be good here:
  # 1) timeseries of calibration data
  # 2) timeseries of calibratino regressions
  # 3) timeseries of ambient data
  # 4) monthly versions of 1-3.
  # all of these should be queried when running this function.
 
  require(ggplot2)
  require(gridExtra)
  
  # query re: calibration plots
  print("This function makes diagnostic plots of calibrated NEON carbon isotope data.")
  
  #-------------------------------------------------------
  # query for which plots.
  which.plots <- menu(c("Raw Calibration data - Monthly","Calibration Parameters - Monthly","Atmospheric Measurements - Monthly",
                        "Raw Calibration data - Full Timeseries","Calibration Parametrs - Full Timeseries","Atmospheric Measurements - Full Timeseries",
                        "All Monthly Plots","All Full Timeseries Plots","All Plots","I've made a huge mistake.")
                      ,title="Which plots should be run?")
  
  # allow graceful exit if want to stop.
  if (which.plots == 10) {
    stop()
  }
  
  #--------------------------------------------------------------
  # If we're still running, make a folder to hold diagnostic plots.
  out_folder <- paste0(plot_path,"/",Sys.time())
  
  dir.create(out_folder)

  #--------------------------------------------------------------
  # Find files common to each plotting script below.
  
  # find the files associated w/ that site.
  if (which.sites == "all") {
    slist <- list.files(data_path,pattern='.h5',recursive=TRUE)  
  } else {   
    # check to see if *is* a neon site.
    neon.sites <- c(NEONiso:::terrestrial_core_sites(),
                    NEONiso:::terrestrial_relocatable_sites())
    
    if (!(which.sites %in% neon.sites)) {
      stop("Invalid NEON site selected!")
    } else {
      slist <- list.files(paste0(data_path,"/",which.sites),pattern='.h5')  
    }
  }
  
  
  # extract lists of domains, site codes, and year-month combos from file names
  slist.tmp <- strsplit(slist,split=".",fixed=TRUE)
  
  domain <- sapply(slist.tmp,'[[',2)
  sitecd <- sapply(slist.tmp,'[[',3)
  yrmn   <- sapply(slist.tmp,'[[',8)

  # validate sitecd. if single site was given, should have 1 unique value.
  if (which.sites != "all") {
    # check to see if length(unique(sitecd)) == 1
    if (length(unique(sitecd)) != 1) {
      stop("Single site selected by user, but multiple sites chosen here")
    }
  }
  
  #--------------------------------------------------------------
  # Set up logical tests to determine which plots below to run.
  # get data.
  
  # get vector of sites:
  unq_sites <- unique(sitecd)
  
  for (i in 1:length(unq_sites)) {
    
    c13_obs_data <- neonUtilities::stackEddy(paste0(data_path,"/",unq_sites[i]),level="dp01",var="dlta13CCo2",avg=9)     
    c13_ref_data <- neonUtilities::stackEddy(paste0(data_path,"/",unq_sites[i]),level="dp01",var="dlta13CCo2Refe",avg=9)     
    co2_obs_data <- neonUtilities::stackEddy(paste0(data_path,"/",unq_sites[i]),level="dp01",var="rtioMoleDryCo2",avg=9)     
    co2_ref_data <- neonUtilities::stackEddy(paste0(data_path,"/",unq_sites[i]),level="dp01",var="rtioMoleDryCo2Refe",avg=9)  
    
    # select data a little more cleverly.
    calData <- list()
    
    test <- c13_obs_data[[1]]
    
    calData[[1]] <- c13_obs_data[[1]] %>%
      dplyr::filter(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch")) %>%
      dplyr::select(timeBgn,timeEnd,data.isoCo2.dlta13CCo2.mean,verticalPosition)%>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd, mean13C = data.isoCo2.dlta13CCo2.mean, standard = verticalPosition)
    
    calData[[2]] <- c13_ref_data[[1]] %>%
      dplyr::filter(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch")) %>%
      dplyr::select(timeBgn,timeEnd,data.isoCo2.dlta13CCo2Refe.mean,verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd, ref13C = data.isoCo2.dlta13CCo2Refe.mean, standard = verticalPosition)

    calData[[3]] <- co2_obs_data[[1]] %>%
      dplyr::filter(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch")) %>%
      dplyr::select(timeBgn,timeEnd,data.isoCo2.rtioMoleDryCo2.mean,verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd, meanCo2 = data.isoCo2.rtioMoleDryCo2.mean, standard = verticalPosition)
    
    calData[[4]] <- co2_ref_data[[1]] %>%
      dplyr::filter(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch")) %>%
      dplyr::select(timeBgn,timeEnd,data.isoCo2.rtioMoleDryCo2Refe.mean,verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd, refCo2 = data.isoCo2.rtioMoleDryCo2Refe.mean, standard = verticalPosition)
    
    calData <- Reduce(function(x,y) merge(x,y,by=c("timeBgn","timeEnd","standard")), calData)
    
    # convert times to POSIXct.
    calData$timeBgn <- as.POSIXct(calData$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
    calData$timeEnd <- as.POSIXct(calData$timeEnd,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")

    #--------------------------------------------------------------
    #--------------------------------------------------------------
    # PLOTTING SCRIPTS LIVE BELOW.
    #--------------------------------------------------------------
    #--------------------------------------------------------------
    
    # 1. Raw calibration data - monthly
    if (which.plots == 1 | which.plots == 7 | which.plots == 9) {
      
      NEONiso:::cplot_monthly_standards(calData,out_folder,unq_sites[i])
      
    }
    
    # 4. Raw calibration data - timeseries
    if (which.plots == 4 | which.plots == 8 | which.plots == 9) {
      
      NEONiso:::cplot_fullts_standards(calData,out_folder,unq_sites[i])
      
    } # if
  } # for unq_sites
} # function

