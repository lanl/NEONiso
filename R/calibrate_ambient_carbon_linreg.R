#' calibrate_ambient_carbon_isotopes
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' Function called by \code{calibrate_ambient_carbon_linreg} to apply
#' gain and offset parameters to the ambient datasets (000_0x0_09m and 000_0x0_30m).
#' This function should generally not be used independently, but should be used 
#' in coordination with \code{calibrate_ambient_carbon_linreg}.
#'
#' @param amb.data.list List containing an ambient d13C dataset. Will include all variables in 000_0x0_xxm. (character)
#' @param caldf Calibration data frame containing gain and offset values for 12C and 13C isotopologues.
#' @param outname Output variable name. Inherited from \code{calibrate_ambient_carbon_linreg}
#' @param site Four-letter NEON code corersponding to site being processed.
#' @param file Output file name. Inherited from \code{calibrate_ambient_carbon_linreg}
#' @param force.to.end In given month, calibrate ambient data later than last calibration, using the last calibration? (default true)
#' @param force.to.beginning In given month, calibrate ambient data before than first calibration, using the first calibration? (default true)
#' @param r2.thres Minimum r2 value for calibration to be considered "good" and applied to ambient data.
#'
#' @return Nothing to environment; returns calibrated ambient observations to the output file. This function is not designed to be called on its own.
#' @export
#'
#' @examples
#' 
#' @importFrom magrittr %>%
#' 
calibrate_ambient_carbon_linreg <- function(amb.data.list,
                                            caldf,
                                            outname,
                                            site,
                                            file,
                                            force.to.end=TRUE,
                                            force.to.beginning=TRUE,
                                            r2.thres=0.95) {

    print("Processing carbon ambient data...")
    
    # only working on the d13C of the amb.data.list, so extract just this...
    d13C_ambdf <- amb.data.list$dlta13CCo2
    co2_ambdf  <- amb.data.list$rtioMoleDryCo2
    
    # ensure that time variables are in POSIXct.
    amb.start.times <- as.POSIXct(d13C_ambdf$timeBgn,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    amb.end.times <- as.POSIXct(d13C_ambdf$timeEnd,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    
    # if force.to.end and/or force.to.beginning are true, match out$start[1] to min(amb time)
    # and/or out$end[nrow] to max(amb time)
    
    if (force.to.end == TRUE) {
      caldf$end[nrow(caldf)] <- amb.end.times[length(amb.end.times)]
    }
    if (force.to.beginning == TRUE) {
      caldf$start[1] <- amb.start.times[1]
    }
    
    # determine which cal period each ambient data belongs to.
    var.inds.in.calperiod <- list()
    
    for (i in 1:nrow(caldf)) {
      int <- lubridate::interval(caldf$start[i],caldf$end[i])
      var.inds.in.calperiod[[i]] <- which(amb.end.times %within% int)
      
      # check to see if calibration point is "valid" - 
      # at present - "valid" means r2 > r2.thres.
      # rpf - 190809.
      # also some gap filling code here! 
      
      if (!is.na(caldf$d13C_r2[i]) & caldf$d13C_r2[i] < r2.thres) {
        # if we're in calibration period 2 or later, carry previous 
        # calibration period forward. else if the first calibration period
        # is bad, find the first good calibration period at index n,
        # and apply to first n periods.
        if (i > 1) {
          caldf$d13C_slope[i] <- caldf$d13C_slope[i-1]
          caldf$d13C_intercept[i] <- caldf$d13C_intercept[i-1]
          caldf$d13C_r2[i] <- caldf$d13C_r2[i-1]
        } else { # i = 1, and need to find first good value.
          first.good.val <- min(which(caldf$d13C_r2 > r2.thres))
          caldf$d13C_slope[i] <- caldf$d13C_slope[first.good.val]
          caldf$d13C_intercept[i] <- caldf$d13C_intercept[first.good.val]
          caldf$d13C_r2[i] <- caldf$d13C_r2[first.good.val]
        }
      }
      
      # apply same logic to CO2 calibration.
      if (!is.na(caldf$co2_r2[i]) & caldf$co2_r2[i] < r2.thres) {
        if (i > 1) {
          caldf$co2_slope[i] <- caldf$co2_slope[i-1]
          caldf$co2_intercept[i] <- caldf$co2_intercept[i-1]
          caldf$co2_r2[i] <- caldf$co2_r2[i-1]
        } else {
          first.good.val <- min(which(caldf$co2_r2 > r2.thres))
          caldf$co2_slope[i] <- caldf$co2_slope[first.good.val]
          caldf$co2_intercept[i] <- caldf$co2_intercept[first.good.val]
          caldf$co2_r2[i] <- caldf$co2_r2[first.good.val]
        }
      }
    }
    
    d13C_ambdf$mean_cal <- d13C_ambdf$mean
    co2_ambdf$mean_cal  <- co2_ambdf$mean
    
    for (i in 1:length(var.inds.in.calperiod)) {
      d13C_ambdf$mean_cal[var.inds.in.calperiod[[i]]] <- round(d13C_ambdf$mean[var.inds.in.calperiod[[i]]]*caldf$d13C_slope[i] + caldf$d13C_intercept[i], digits = 2)
      d13C_ambdf$min[var.inds.in.calperiod[[i]]]  <- round(d13C_ambdf$min[var.inds.in.calperiod[[i]]]*caldf$d13C_slope[i] + caldf$d13C_intercept[i], digits = 2)
      d13C_ambdf$max[var.inds.in.calperiod[[i]]]  <- round(d13C_ambdf$max[var.inds.in.calperiod[[i]]]*caldf$d13C_slope[i] + caldf$d13C_intercept[i], digits = 2)
      
      
      co2_ambdf$mean_cal[var.inds.in.calperiod[[i]]] <- round(co2_ambdf$mean[var.inds.in.calperiod[[i]]]*caldf$co2_slope[i] + caldf$co2_intercept[i], digits = 2)
      co2_ambdf$min[var.inds.in.calperiod[[i]]]  <- round(co2_ambdf$min[var.inds.in.calperiod[[i]]]*caldf$co2_slope[i] + caldf$co2_intercept[i], digits = 2)
      co2_ambdf$max[var.inds.in.calperiod[[i]]]  <- round(co2_ambdf$max[var.inds.in.calperiod[[i]]]*caldf$co2_slope[i] + caldf$co2_intercept[i], digits = 2)
    }
    
    # round variance down to 2 digits
    d13C_ambdf$vari <- round(d13C_ambdf$vari, digits = 2)
    co2_ambdf$vari <- round(co2_ambdf$vari, digits = 2)
    
    # replace ambdf in amb.data.list, return amb.data.list
    amb.data.list$dlta13CCo2 <- d13C_ambdf
    amb.data.list$rtioMoleDryCo2 <- co2_ambdf
    
    # write out dataset to HDF5 file.
    fid <- rhdf5::H5Fopen(file)
    
    co2.data.outloc <- rhdf5::H5Gcreate(fid,paste0('/',site,'/dp01/data/isoCo2/',outname))
    
    # loop through each of the variables in list amb.data.list and write out as a dataframe.
    lapply(names(amb.data.list),function(x) {
      rhdf5::h5writeDataset.data.frame(obj=amb.data.list[[x]],
                                h5loc=co2.data.outloc,
                                name=x,
                                DataFrameAsCompound = TRUE)})
    
    # close all open handles.
    rhdf5::h5closeAll()
}
