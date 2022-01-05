#' calibrate_ambient_water_isotopes
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' Function called by `calibrate_ambient_water_linreg` to apply
#' slope and intercept parameters to the ambient datasets (000_0x0_09m and
#' 000_0x0_30m) to correct to the VSMOW scale.
#' This function should generally not be used independently,
#' but should be used with `calibrate_ambient_water_linreg`.
#' Note that in this version *NO CORRECTION FOR HUMIDITY* is performed.
#' Use with caution.
#'
#' @param amb_data_list List containing ambient d18O/d2H datasets.
#'             Will include all variables in 000_0x0_xxm. (character)
#' @param caldf Calibration data frame containing slope and intercept values
#'             for d18O and d2H values.
#' @param outname Output variable name. Inherited from
#'             `calibrate_ambient_water_linreg`
#' @param site Four-letter NEON code corresponding to site being processed.
#' @param file Output file name. Inherited from
#'             `calibrate_ambient_water_linreg`
#' @param force_to_end In given month, calibrate ambient data later than last
#'             calibration, using the last calibration? (default true)
#' @param force_to_beginning In given month, calibrate ambient data before than
#'             first calibration, using the first calibration? (default true)
#' @param r2_thres Minimum r2 value for calibration to be considered "good" and
#'             applied to ambient data.
#' @param filter_data Apply a median filter to output ambient data? inherited.
#'
#' @return Nothing to environment; returns calibrated ambient observations to
#'     the output file. This function is not designed to be called on its own.
#' @export
#'
#' @importFrom magrittr %>%
calibrate_ambient_water_linreg <- function(amb_data_list,
                                           caldf,
                                           outname,
                                           site,
                                           file,
                                           filter_data,
                                           force_to_end,
                                           force_to_beginning,
                                           r2_thres) {
  
  # print status.
  print("Processing water ambient data...")
  
  # In contrast to carbon calibration - need to get both 18O and 2H separately
  oxydf <- amb_data_list$dlta18OH2o
  hyddf <- amb_data_list$dlta2HH2o
  
  #-------------------------------------------------------
  # oxygen.
  #-------------------------------------------------------
  # ensure that time variables are in POSIXct.
  # (note: these should be the same for 18O and 2H?)
  amb_start_times <- convert_NEONhdf5_to_POSIXct_time(oxydf$timeBgn)
  amb_end_times <- convert_NEONhdf5_to_POSIXct_time(oxydf$timeEnd)

  # if force.to.end and/or force.to.beginning are true, match out$start[1]
  # to min(amb time) and/or out$end[nrow] to max(amb time)
  
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
  
  # calibrate data at this height.
  oxydf$mean_cal <- oxydf$mean
  oxydf$max_cal  <- oxydf$max
  oxydf$min_cal  <- oxydf$min
  
  for (i in 1:length(var_inds_in_calperiod)) {
    if (!is.na(caldf$o_r2[i]) & caldf$o_r2[i] > r2_thres) {
      
      oxydf$mean_cal[var_inds_in_calperiod[[i]]] <- caldf$o_intercept[i] +
        oxydf$mean[var_inds_in_calperiod[[i]]] * caldf$o_slope[i]
      oxydf$min_cal[var_inds_in_calperiod[[i]]] <- caldf$o_intercept[i] +
        oxydf$min[var_inds_in_calperiod[[i]]] * caldf$o_slope[i]
      oxydf$max_cal[var_inds_in_calperiod[[i]]] <- caldf$o_intercept[i] +
        oxydf$max[var_inds_in_calperiod[[i]]] * caldf$o_slope[i]
      
    } else {
      
      oxydf$mean_cal[var_inds_in_calperiod[[i]]] <- NA
      oxydf$min_cal[var_inds_in_calperiod[[i]]]  <- NA
      oxydf$max_cal[var_inds_in_calperiod[[i]]]  <- NA
      
    }
    
  }
  
  # apply median filter to data
  if (filter_data == TRUE) {
    oxydf$mean_cal     <- filter_median_Brock86(oxydf$mean_cal)
    oxydf$min_cal      <- filter_median_Brock86(oxydf$min_cal)
    oxydf$max_cal      <- filter_median_Brock86(oxydf$max_cal)
  }

  oxydf$timeBgn <- convert_POSIXct_to_NEONhdf5_time(oxydf$timeBgn)
  oxydf$timeEnd <- convert_POSIXct_to_NEONhdf5_time(oxydf$timeEnd)
  
  # replace ambdf in amb_data_list
  amb_data_list$dlta18OH2o <- oxydf
  
  #-------------------------------------------------------
  # hydrogen.
  #-------------------------------------------------------
  rm(amb_start_times, oxydf, amb_end_times, i, var_inds_in_calperiod)
  
  # ensure that time variables are in POSIXct.
  amb_start_times <- convert_NEONhdf5_to_POSIXct_time(hyddf$timeBgn)
  amb_end_times   <- convert_NEONhdf5_to_POSIXct_time(hyddf$timeEnd)
  
  # if force.to.end and/or force.to.beginning are true,
  # match out$start[1] to min(amb time) and/or out$end[nrow] to max(amb time)
  
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
    
    if (!is.na(caldf$h_r2[i]) & caldf$h_r2[i] > r2_thres) {
      
      hyddf$mean_cal[var_inds_in_calperiod[[i]]] <- caldf$h_intercept[i] +
        hyddf$mean[var_inds_in_calperiod[[i]]] * caldf$h_slope[i]
      hyddf$min_cal[var_inds_in_calperiod[[i]]] <- caldf$h_intercept[i] +
        hyddf$min[var_inds_in_calperiod[[i]]] * caldf$h_slope[i]
      hyddf$max_cal[var_inds_in_calperiod[[i]]] <- caldf$h_intercept[i] +
        hyddf$max[var_inds_in_calperiod[[i]]] * caldf$h_slope[i]
      
    } else {
      
      hyddf$mean_cal[var_inds_in_calperiod[[i]]] <- NA
      hyddf$min_cal[var_inds_in_calperiod[[i]]] <- NA
      hyddf$max_cal[var_inds_in_calperiod[[i]]] <- NA
      
    }
    
  }
  
  # apply median filter to data
  if (filter_data == TRUE) {
    hyddf$mean_cal <- filter_median_Brock86(hyddf$mean_cal)
    hyddf$min_cal  <- filter_median_Brock86(hyddf$min_cal)
    hyddf$max_cal  <- filter_median_Brock86(hyddf$max_cal)
  }
  
  hyddf$timeBgn <- convert_POSIXct_to_NEONhdf5_time(hyddf$timeBgn)
  hyddf$timeEnd <- convert_POSIXct_to_NEONhdf5_time(hyddf$timeEnd)
  
  # replace ambdf in amb_data_list
  amb_data_list$dlta2HH2o <- hyddf
  
  #-----------------------------------------------------------
  # write out dataset to HDF5 file.
  fid <- rhdf5::H5Fopen(file)
  
  print(outname)
  h2o_data_outloc <- rhdf5::H5Gcreate(fid,
                                      paste0("/", site, "/dp01/data/isoH2o/", outname))
  
  # loop through variables in list amb_data_list and write out as a dataframe.
  lapply(names(amb_data_list), function(x) {
    rhdf5::h5writeDataset(obj = amb_data_list[[x]],
                                     h5loc = h2o_data_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  # close all open handles.
  rhdf5::h5closeAll()
}
