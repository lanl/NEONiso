#' calibrate_ambient_carbon_Bowling2003
#'
#' Function called by \code{calibrate_ambient_carbon_Bowling2003} to apply
#' gain and offset parameters to the ambient datasets (000_0x0_09m and 000_0x0_30m).
#' This function should generally not be used independently, but should be used 
#' in coordination with \code{calibrate_ambient_carbon_Bowling2003}.
#'
#' @param amb.data.list List containing an ambient d13C dataset. Will include all variables in 000_0x0_xxm. (character)
#' @param caldf Calibration data frame containing gain and offset values for 12C and 13C isotopologues. 
#' @param outname Output variable name. Inherited from \code{calibrate_ambient_carbon_Bowling2003}
#' @param site Four-letter NEON code corersponding to site being processed.
#' @param file Output file name. Inherited from \code{calibrate_ambient_carbon_Bowling2003}
#' @param filter.data Apply median absolute deviation filter from Brock 86 to remove impulse spikes? Inherited from \code{calibrate_ambient_carbon_Bowling2003}
#'
#' @return Nothing to environment; returns calibrated ambient observations to the output file. This function is not designed to be called on its own.
#' @export
#'
#' 
calibrate_ambient_carbon_Bowling2003 <- function(amb.data.list,
                                                 caldf,
                                                 outname,
                                                 site,
                                                 file,
                                                 filter.data) {
  
  #-----------------------------------------------------------
  # specify a few parameters for the Bowling method.
  
  f <- 0.00474  # fraction of CO2 isotopomers that aren't 12CO2 or 13CO2
  # note: f technically varies, but this has little impact
  # on calibration per Griffis et al. 2004.
  
  R_vpdb <- 0.0111797 # 13C/12C ratio for VPD standard.
  
  # should be able to get a calGainsOffsets object from the H5 file.
  
  # only working on the d13C of the amb.data.list, so extract just this...
  amb.delta <- amb.data.list$dlta13CCo2
  amb.CO2   <- amb.data.list$rtioMoleDryCo2
  
  # instead of using the [12CO2] and [13CO2] values, calculate from the isotope
  # ratio instead.
  amb.12CO2 <- amb.13CO2 <- amb.CO2 # this is bad practice, but okay to start w/.
  
  amb.12CO2$mean <- amb.CO2$mean*(1-f)/(1+R_vpdb*(1+amb.delta$mean/1000))
  amb.13CO2$mean <- amb.CO2$mean*(1-f) - amb.12CO2$mean
  
  # ensure that time variables are in POSIXct.
  amb.start.times <- as.POSIXct(amb.delta$timeBgn,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  amb.end.times <- as.POSIXct(amb.delta$timeEnd,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  
  # determine which cal period each ambient data belongs to.
  var.inds.in.calperiod <- list()
  
  for (i in 1:nrow(caldf)) {
    int <- interval(caldf$start[i],caldf$end[i])
    var.inds.in.calperiod[[i]] <- which(amb.end.times %within% int)
  }
  
  # calibrate data at this height.
  #-------------------------------------
  # extract 12CO2 and 13CO2 concentrations from the ambient data
  
  mean12C <- amb.delta$mean # create placeholders for 12CO2 vectors
  mean13C <- amb.delta$mean # create placeholders for 13CO2 vectors
  amb.delta$mean_cal <- amb.delta$qflag2 <- amb.delta$qflag1 <- amb.delta$mean # placeholders for calibrated delta vals.
  
  for (i in 1:length(var.inds.in.calperiod)) {
    # calculate calibrated 12CO2 concentrations
    mean12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i]*amb.12CO2$mean[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
    
    # calculate calibrated 13CO2 concentrations
    mean13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i]*amb.13CO2$mean[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]
  }
  
  # output calibrated delta values.
  amb.delta$mean_cal <- 1000*(mean13C/mean12C/R_vpdb - 1)
  
  # apply median filter to data
  if (filter.data == TRUE) {
    amb.delta$mean_cal <- filter_median_Brock86(amb.delta$mean_cal)
  }
  
  # replace ambdf in amb.data.list, return amb.data.list
  amb.data.list$dlta13CCo2 <- amb.delta
  
  # trap to see if qflag1, qflag2, or mean_cal is ever a logical.
  if (class(amb.delta$qflag1) == "logical" |
      class(amb.delta$qflag2) == "logical" |
      class(amb.delta$mean_cal) == "logical") {
    stop("Incorrect class in ambient dataset being written to hdf5 - make sure all logicals are numeric.")
  }
  
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
  
  H5Gclose(co2.data.outloc)
  H5Fclose(fid)
  # close all open handles.
  h5closeAll()
  
  # seems to be a problem where closed file handles aren't registering....so introduce a short break.
  #Sys.sleep(0.5) 
}