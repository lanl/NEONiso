#' calibrate_ambient_water_isotopes
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' Function called by \code{calibrate_ambient_water_linreg} to apply
#' slope and intercept parameters to the ambient datasets (000_0x0_09m and
#' 000_0x0_30m) to correct to the VSMOW scale.
#' This function should generally not be used independently,
#' but should be used with \code{calibrate_ambient_water_linreg}.
#' Note that in this version *NO CORRECTION FOR HUMIDITY* is performed.
#' Use with caution.
#'
#' @param amb_data_list List containing ambient d18O/d2H datasets.
#'             Will include all variables in 000_0x0_xxm. (character)
#' @param caldf Calibration data frame containing slope and intercept values
#'             for d18O and d2H values.
#' @param outname Output variable name. Inherited from
#'             \code{calibrate_ambient_water_linreg}
#' @param site Four-letter NEON code corersponding to site being processed.
#' @param file Output file name. Inherited from
#'             \code{calibrate_ambient_water_linreg}
#' @param force_to_end In given month, calibrate ambient data later than last
#'             calibration, using the last calibration? (default true)
#' @param force_to_beginning In given month, calibrate ambient data before than
#'             first calibration, using the first calibration? (default true)
#' @param r2_thres Minimum r2 value for calibration to be considered "good" and
#'             applied to ambient data.
#'
#' @return Nothing to environment; returns calibrated ambient observations to
#'     the output file. This function is not designed to be called on its own.
#' @export
#'
#' @examples
#' 
#' @importFrom magrittr %>%
#'  
calibrate_ambient_water_linreg <- function(amb_data_list,
                                           caldf,
                                           outname,
                                           site,
                                           file,
                                           force_to_end = TRUE, 
                                           force_to_beginning = TRUE,
                                           r2_thres = 0.95) {
  
  # print status.
  print("Processing water ambient data...")
  
  # In contrast to carbon calibration - need to get both 18O and 2H separately...
  oxydf <- amb_data_list$dlta18OH2o
  hyddf <- amb_data_list$dlta2HH2o
  
  # 190103 - rpf. need separate processing for oxygen and hydrogen in case there are a different 
  # number of rows between oxygen and hydrogen. not sure yet why this would arise, but it does 
  # appear in the NEON archive (OSBS).
  #-------------------------------------------------------
  # oxygen.
  #-------------------------------------------------------
  # ensure that time variables are in POSIXct. (note: these should be the same for 18O and 2H?)
  amb_start_times <- as.POSIXct(oxydf$timeBgn, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  amb_end_times <- as.POSIXct(oxydf$timeEnd, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  
  # if force.to.end and/or force.to.beginning are true, match out$start[1] to min(amb time)
  # and/or out$end[nrow] to max(amb time)
  
  if (force_to_end == TRUE) {
    caldf$end[nrow(caldf)] <- amb_end_times[length(amb_end_times)]
  }
  if (force_to_beginning == TRUE) {
    caldf$start[1] <- amb_start_times[1]
  }
  
  # determine which cal period each ambient data belongs to.
  var_inds_in_calperiod <- list()
  
  for (i in 1:nrow(caldf)) {
    int <- interval(caldf$start[i], caldf$end[i])
    var_inds_in_calperiod[[i]] <- which(amb_end_times %within% int)
  }
  
  # calibrate data at this height.
  oxydf$mean_cal <- oxydf$mean
  oxydf$max_cal  <- oxydf$max
  oxydf$min_cal  <- oxydf$min 
  
  for (i in 1:length(var_inds_in_calperiod)) {
    oxydf$mean_cal[var_inds_in_calperiod[[i]]] <- oxydf$mean[var_inds_in_calperiod[[i]]] * caldf$o_slope[i] + caldf$o_intercept[i]
    oxydf$min_cal[var_inds_in_calperiod[[i]]] <- oxydf$min[var_inds_in_calperiod[[i]]] * caldf$o_slope[i] + caldf$o_intercept[i]
    oxydf$max_cal[var_inds_in_calperiod[[i]]] <- oxydf$max[var_inds_in_calperiod[[i]]] * caldf$o_slope[i] + caldf$o_intercept[i]
    
  }
  
  # replace ambdf in amb_data_list
  amb_data_list$dlta18OH2o <- oxydf
  
  #-------------------------------------------------------
  # hydrogen.
  #-------------------------------------------------------
  rm(amb_start_times, oxydf, amb_end_times, i, var_inds_in_calperiod)
  
  # ensure that time variables are in POSIXct. (note: these should be the same for 18O and 2H?)
  amb_start_times <- as.POSIXct(hyddf$timeBgn, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  amb_end_times <- as.POSIXct(hyddf$timeEnd, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  
  # if force.to.end and/or force.to.beginning are true, match out$start[1] to min(amb time)
  # and/or out$end[nrow] to max(amb time)
  
  if (force_to_end == TRUE) {
    caldf$end[nrow(caldf)] <- amb_end_times[length(amb_end_times)]
  }
  if (force_to_beginning == TRUE) {
    caldf$start[1] <- amb_start_times[1]
  }
  
  # determine which cal period each ambient data belongs to.
  var_inds_in_calperiod <- list()
  
  for (i in 1:nrow(caldf)) {
    int <- lubridate::interval(caldf$start[i], caldf$end[i])
    var_inds_in_calperiod[[i]] <- which(amb_end_times %within% int)
  } 
  
  hyddf$mean_cal <- hyddf$mean
  hyddf$max_cal  <- hyddf$max
  hyddf$min_cal  <- hyddf$min 
  
  for (i in 1:length(var_inds_in_calperiod)) {
    hyddf$mean_cal[var_inds_in_calperiod[[i]]] <- hyddf$mean[var_inds_in_calperiod[[i]]] * caldf$h_slope[i] + caldf$h_intercept[i]
    hyddf$min_cal[var_inds_in_calperiod[[i]]] <- hyddf$min[var_inds_in_calperiod[[i]]] * caldf$h_slope[i] + caldf$h_intercept[i]
    hyddf$max_cal[var_inds_in_calperiod[[i]]] <- hyddf$max[var_inds_in_calperiod[[i]]] * caldf$h_slope[i] + caldf$h_intercept[i]
  }
  
  # replace ambdf in amb_data_list
  amb_data_list$dlta2HH2o <- hyddf
  
  #-----------------------------------------------------------
  # write out dataset to HDF5 file.
  fid <- rhdf5::H5Fopen(file)
  
  print(outname)
  h2o_data_outloc <- rhdf5::H5Gcreate(fid, paste0("/", site, "/dp01/data/isoH2o/", outname))
  
  # loop through each of the variables in list amb_data_list and write out as a dataframe.
  lapply(names(amb_data_list), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = amb_data_list[[x]],
                                     h5loc = h2o_data_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  # close all open handles.
  rhdf5::h5closeAll()
}
