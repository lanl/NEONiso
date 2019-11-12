#' calibrate_ambient_carbon_Bowling2003
#'
#' @param amb.data.list 
#' @param caldf Calibration data frame containing gain and offset values for 12C and 13C isotopologues.
#' @param outname Output file name.
#' @param site Four-letter NEON code corersponding to site being processed.
#' @param file 
#' @param force.to.end Extend last good calibration to end of dataset??
#' @param force.to.beginning Enforce a valid calibration at the beginning of the record by carrying first good calibration back to beginning of record? (Default = TRUE)
#'
#' @return Nothing to environment; returns calibrated ambient observations to the calibrate_carbon_Bowling2003 function. This function is not designed to be called on its own.
#' @export
#'
#' @examples
#' 
calibrate_ambient_carbon_Bowling2003 <- function(amb.data.list,caldf,outname,site,file,forceToEnd=TRUE,forceToBeginning=TRUE,
                                                 carryLastGoodCal=TRUE) {
  
  # required libraries
  require(rhdf5)
  require(zoo)
  
  #-----------------------------------------------------------
  # specify a few parameters for the Bowling method.
  
  f <- 0.00474  # fraction of CO2 isotopomers that aren't 12CO2 or 13CO2
  # note: f technically varies, but this has little impact
  # on calibration per Griffis et al. 2004.
  
  R_vpdb <- 0.0111797 # 13C/12C ratio for VPD standard.
  
  # Method 1 refers to the Bowling et al. 2003 calibratino technique.
  # should be able to get a calGainsOffsets object from the H5 file.
  
  # only working on the d13C of the amb.data.list, so extract just this...
  amb.delta <- amb.data.list$dlta13CCo2
  amb.CO2   <- amb.data.list$rtioMoleDryCo2
  # instead of using the [12CO2] and [13CO2] values, calculate from the isotope
  # ratio instead.
  amb.12CO2 <- amb.13CO2 <- amb.CO2 # this is bad practice, but okay to start w/.
  
  amb.12CO2$mean <- amb.CO2$mean*(1-f)/(1+R_vpdb*(1+amb.delta$mean/1000))
  amb.13CO2$mean <- amb.CO2$mean*(1-f) - amb.12CO2$mean
  
  #amb.12CO2 <- amb.data.list$rtioMoleDry12CCo2
  #amb.13CO2 <- amb.data.list$rtioMoleDry13CCo2
  
  # ensure that time variables are in POSIXct.
  amb.start.times <- as.POSIXct(amb.delta$timeBgn,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  amb.end.times <- as.POSIXct(amb.delta$timeEnd,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  
  # determine which cal period each ambient data belongs to.
  var.inds.in.calperiod <- list()
  
  # require NAs to be removed.
  if (carryLastGoodCal == TRUE) {
    caldf <- na.locf(caldf,na.rm=FALSE)
  }
  
  for (i in 1:nrow(caldf)) {
    int <- interval(caldf$start[i],caldf$end[i])
    var.inds.in.calperiod[[i]] <- which(amb.end.times %within% int)
  }
  
  # calibrate data at this height.
  #-------------------------------------
  # extract 12CO2 and 13CO2 concentrations from the ambient data
  
  mean12C <- vari12C <- amb.delta$mean # create placeholders for 12CO2 vectors
  mean13C <- vari13C <- amb.delta$mean # create placeholders for 13CO2 vectors
  amb.delta$mean_cal <- amb.delta$qflag2 <- amb.delta$qflag1 <- amb.delta$mean # placeholders for calibrated delta vals.
  
  for (i in 1:length(var.inds.in.calperiod)) {
    # calculate calibrated 12CO2 concentrations
    mean12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i]*amb.12CO2$mean[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
    vari12C[var.inds.in.calperiod[[i]]] <- caldf$vari.g12C[i]*amb.12CO2$mean[var.inds.in.calperiod[[i]]]^2 + caldf$vari.o12C[i]
    #min12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i]*amb.12CO2$min[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
    #max12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i]*amb.12CO2$max[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
    
    # calculate calibrated 13CO2 concentrations
    mean13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i]*amb.13CO2$mean[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]
    vari13C[var.inds.in.calperiod[[i]]] <- caldf$vari.g13C[i]*amb.13CO2$mean[var.inds.in.calperiod[[i]]]^2 + caldf$vari.o13C[i]
    #min13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i]*amb.13CO2$min[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]
    #max13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i]*amb.13CO2$max[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]
    
    # copy over quality flag to indicate where the calibration seems to be good.
    amb.delta$qflag1[var.inds.in.calperiod[[i]]] <- caldf$calVal.flag1[var.inds.in.calperiod[[i]]]
    amb.delta$qflag2[var.inds.in.calperiod[[i]]] <- caldf$calVal.flag2[var.inds.in.calperiod[[i]]]
  }
  
  # output calibrated delta values.
  amb.delta$mean_cal <- 1000*(mean13C/mean12C/R_vpdb - 1)
  amb.delta$mean12CCO2 <- mean12C
  amb.delta$mean13CCO2 <- mean13C
  amb.delta$vari12CCO2 <- vari12C
  amb.delta$vari13CCO2 <- vari13C
  amb.delta$vari_cal <- 1000^2*(mean13C/mean12C)^2*(vari12C/(sqrt(312)*amb.12CO2$mean^2) + vari13C/(sqrt(312)*amb.13CO2$mean^2))
  
  #amb.delta$min_cal <- 1000*(min13C/min12C/R_vpdb - 1)
  #amb.delta$max_cal <- 1000*(max13C/max12C/R_vpdb - 1)
  
  # replace ambdf in amb.data.list, return amb.data.list
  amb.data.list$dlta13CCo2 <- amb.delta
  
  # write out dataset to HDF5 file.
  fid <- H5Fopen(file)
  
  #print(outname)
  co2.data.outloc <- H5Gcreate(fid,paste0('/',site,'/dp01/data/isoCo2/',outname))
  
  # loop through each of the variables in list amb.data.list and write out as a dataframe.
  lapply(names(amb.data.list),function(x) {
    h5writeDataset.data.frame(obj=amb.data.list[[x]],
                              h5loc=co2.data.outloc,
                              name=x,
                              DataFrameAsCompound = TRUE)})
  
  # close all open handles.
  h5closeAll()
  
  # seems to be a problem where closed file handles aren't registering....so introduce a short break.
  Sys.sleep(0.5) 
}