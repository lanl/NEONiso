#' calibrate_ambient_carbon_Bowling2003
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
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
#' @param force.to.end In given month, calibrate ambient data later than last calibration, using the last calibration? (default true)
#' @param force.to.beginning In given month, calibrate ambient data before than first calibration, using the first calibration? (default true)
#' @param r2.thres Minimum r2 value for calibration to be considered "good" and applied to ambient data.
#'
#' @return Nothing to environment; returns calibrated ambient observations to the output file. This function is not designed to be called on its own.
#' @export
#'
#' @importFrom magrittr %>%
#' 
calibrate_ambient_carbon_Bowling2003 <- function(amb.data.list,
                                                 caldf,
                                                 outname,
                                                 site,
                                                 file,
                                                 filter.data,
                                                 force.to.end,
                                                 force.to.beginning,
                                                 r2.thres=0.95) {
  
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
  
  amb.12CO2$mean <- amb.CO2$mean * ( 1 - f ) / 
    ( 1 + R_vpdb * ( 1 + amb.delta$mean / 1000))
  amb.13CO2$mean <- amb.CO2$mean * (1-f) - amb.12CO2$mean
  
  amb.12CO2$min <- amb.CO2$min * (1-f) / 
    (1 + R_vpdb * (1 + amb.delta$min / 1000))
  amb.13CO2$min <- amb.CO2$min * (1-f) - amb.12CO2$min
  
  amb.12CO2$max <- amb.CO2$max * (1-f) / 
    (1 + R_vpdb * (1 + amb.delta$max / 1000))
  amb.13CO2$max <- amb.CO2$max * (1-f) - amb.12CO2$max
  
  # ensure that time variables are in POSIXct.
  amb.start.times <- as.POSIXct(amb.delta$timeBgn, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  amb.end.times <- as.POSIXct(amb.delta$timeEnd, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  
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
  
  # determine which cal period each ambient data belongs to.
  var.inds.in.calperiod <- list()
  
  for (i in 1:nrow(caldf)) {
    int <- lubridate::interval(caldf$start[i], caldf$end[i])
    var.inds.in.calperiod[[i]] <- which(amb.end.times %within% int)
    
    # check to see if calibration point is "valid" - 
    # at present - "valid" means r2 > r2.thres.
    # rpf - 190809.
    # also some gap filling code here! 
    
    # 12CO2 calibration paramters.
    if (!is.na(caldf$r2_12C[i]) & caldf$r2_12C[i] < r2.thres) {
      # if we're in calibration period 2 or later, carry previous 
      # calibration period forward. else if the first calibration period
      # is bad, find the first good calibration period at index n,
      # and apply to first n periods.
      if (i > 1) {
        caldf$gain12C[i] <- caldf$gain12C[i-1]
        caldf$offset12C[i] <- caldf$offset12C[i-1]
        caldf$r2_12C[i] <- caldf$r2_12C[i-1]
      } else { # i = 1, and need to find first good value.
        first.good.val <- min(which(caldf$r2_12C > r2.thres))
        caldf$gain12C[i] <- caldf$gain12C[first.good.val]
        caldf$offset12C[i] <- caldf$offset12C[first.good.val]
        caldf$r2_12C[i] <- caldf$r2_12C[first.good.val]
      }
    }
    
    # 13CO2 calibration parameters - equivalent logic to 12Co2.
    if (!is.na(caldf$r2_13C[i]) & caldf$r2_13C[i] < r2.thres) {
      if (i > 1) {
        caldf$gain13C[i] <- caldf$gain13C[i-1]
        caldf$offset13C[i] <- caldf$offset13C[i-1]
        caldf$r2_13C[i] <- caldf$r2_13C[i-1]
      } else {
        first.good.val <- min(which(caldf$r2_13C > r2.thres))
        caldf$gain13C[i] <- caldf$gain13C[first.good.val]
        caldf$offset13C[i] <- caldf$offset13C[first.good.val]
        caldf$r2_13C[i] <- caldf$r2_13C[first.good.val]
      }
    }
  }
  
  # calibrate data at this height.
  #-------------------------------------
  # extract 12CO2 and 13CO2 concentrations from the ambient data
  mean12C <- max12C <- min12C <- amb.delta$mean # create placeholders for 12CO2 vectors
  mean13C <- max13C <- min13C <- amb.delta$mean # create placeholders for 13CO2 vectors
  
  amb.CO2$mean_cal <- amb.delta$mean
  
  for (i in 1:length(var.inds.in.calperiod)) {
    # calculate calibrated 12CO2 concentrations
    mean12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i] * amb.12CO2$mean[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
    min12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i] * amb.12CO2$min[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
    max12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i] * amb.12CO2$max[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
    
    # calculate calibrated 13CO2 concentrations
    mean13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i] * amb.13CO2$mean[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]
    min13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i] * amb.13CO2$min[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]
    max13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i] * amb.13CO2$max[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]

  }
  
  # output calibrated delta values.
  amb.delta$mean_cal <- round(1000 * (mean13C / mean12C / R_vpdb - 1), 2)
  amb.delta$min  <- round(1000 * (min13C / min12C / R_vpdb - 1), 2)
  amb.delta$max  <- round(1000 * (max13C / max12C / R_vpdb - 1), 2)
  amb.delta$vari <- round(amb.delta$vari, 2)

  # calibrate co2 mole fractions.
  amb.CO2$mean_cal <- (mean13C + mean12C) / (1-f)
  
  # apply median filter to data
  if (filter.data == TRUE) {
    amb.delta$mean_cal <- filter_median_Brock86(amb.delta$mean_cal)
    amb.delta$min      <- filter_median_Brock86(amb.delta$min)
    amb.delta$max      <- filter_median_Brock86(amb.delta$max)
  }
  
  # replace ambdf in amb.data.list, return amb.data.list
  amb.data.list$dlta13CCo2 <- amb.delta

  amb.data.list$rtioMoleDryCo2 <- amb.CO2
  
  # write out dataset to HDF5 file.
  fid <- rhdf5::H5Fopen(file)
  
  #print(outname)
  co2.data.outloc <- rhdf5::H5Gcreate(fid, paste0("/", site, "/dp01/data/isoCo2/", outname))
  
  # loop through each of the variables in list amb.data.list and write out as a dataframe.
  lapply(names(amb.data.list), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = amb.data.list[[x]], 
                              h5loc = co2.data.outloc,
                              name = x,
                              DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(co2.data.outloc)
  rhdf5::H5Fclose(fid)
  # close all open handles.
  rhdf5::h5closeAll()
  
  # seems to be a problem where closed file handles aren't registering....so introduce a short break.
  #Sys.sleep(0.5) 
}