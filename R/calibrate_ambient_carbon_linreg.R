#' calibrate_ambient_carbon_linreg
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' Function called by \code{calibrate_ambient_carbon_linreg} to apply
#' gain and offset parameters to the ambient datasets (000_0x0_09m and
#' 000_0x0_30m). This function should generally not be used independently,
#' but should be used with \code{calibrate_ambient_carbon_linreg}.
#'
#' @param amb_data_list List containing an ambient d13C dataset.
#'             Will include all variables in 000_0x0_xxm. (character)
#' @param caldf Calibration data frame containing gain and offset values for
#'             12C and 13C isotopologues.
#' @param outname Output variable name. Inherited from
#'             \code{calibrate_ambient_carbon_linreg}
#' @param site Four-letter NEON code corersponding to site being processed.
#' @param file Output file name. Inherited from
#'             \code{calibrate_ambient_carbon_linreg}
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
#' @importFrom magrittr %>%
#'
calibrate_ambient_carbon_linreg <- function(amb_data_list,
                                            caldf,
                                            outname,
                                            site,
                                            file,
                                            force_to_end=TRUE,
                                            force_to_beginning=TRUE,
                                            r2_thres) {

    print("Processing carbon ambient data...")

    # only working on the d13C of the amb.data.list, so extract just this...
    d13C_ambdf <- amb_data_list$dlta13CCo2
    co2_ambdf  <- amb_data_list$rtioMoleDryCo2

    # ensure that time variables are in POSIXct.
    amb_start_times <- as.POSIXct(d13C_ambdf$timeBgn,
                                  format = "%Y-%m-%dT%H:%M:%OSZ",
                                  tz = "UTC")
    amb_end_times <- as.POSIXct(d13C_ambdf$timeEnd,
                                format = "%Y-%m-%dT%H:%M:%OSZ",
                                tz = "UTC")

    # if force.to.end and/or force.to.beginning are true,
    # match out$start[1] to min(amb time)
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

      # check to see if calibration point is "valid" -
      # at present - "valid" means r2 > r2.thres.
      # rpf - 190809.
      # also some gap filling code here!

      if (!is.na(caldf$d13C_r2[i]) & caldf$d13C_r2[i] < r2_thres) {
        # if we're in calibration period 2 or later, carry previous
        # calibration period forward. else if the first calibration period
        # is bad, find the first good calibration period at index n,
        # and apply to first n periods.
        if (i > 1) {
          caldf$d13C_slope[i] <- caldf$d13C_slope[i - 1]
          caldf$d13C_intercept[i] <- caldf$d13C_intercept[i - 1]
          caldf$d13C_r2[i] <- caldf$d13C_r2[i - 1]
        } else { # i = 1, and need to find first good value.
          first_good_val <- min(which(caldf$d13C_r2 > r2_thres))
          caldf$d13C_slope[i] <- caldf$d13C_slope[first_good_val]
          caldf$d13C_intercept[i] <- caldf$d13C_intercept[first_good_val]
          caldf$d13C_r2[i] <- caldf$d13C_r2[first_good_val]
        }
      }

      # apply same logic to CO2 calibration.
      if (!is.na(caldf$co2_r2[i]) & caldf$co2_r2[i] < r2_thres) {
        if (i > 1) {
          caldf$co2_slope[i] <- caldf$co2_slope[i - 1]
          caldf$co2_intercept[i] <- caldf$co2_intercept[i - 1]
          caldf$co2_r2[i] <- caldf$co2_r2[i - 1]
        } else {
          first_good_val <- min(which(caldf$co2_r2 > r2_thres))
          caldf$co2_slope[i] <- caldf$co2_slope[first_good_val]
          caldf$co2_intercept[i] <- caldf$co2_intercept[first_good_val]
          caldf$co2_r2[i] <- caldf$co2_r2[first_good_val]
        }
      }
    }

    d13C_ambdf$mean_cal <- d13C_ambdf$mean
    co2_ambdf$mean_cal  <- co2_ambdf$mean

    for (i in 1:length(var_inds_in_calperiod)) {

      d13C_ambdf$mean_cal[var_inds_in_calperiod[[i]]] <- caldf$d13C_intercept[i] +
        d13C_ambdf$mean[var_inds_in_calperiod[[i]]] * caldf$d13C_slope[i]

      d13C_ambdf$min[var_inds_in_calperiod[[i]]]  <- caldf$d13C_intercept[i] +
        d13C_ambdf$min[var_inds_in_calperiod[[i]]] * caldf$d13C_slope[i]

      d13C_ambdf$max[var_inds_in_calperiod[[i]]]  <- caldf$d13C_intercept[i] +
        d13C_ambdf$max[var_inds_in_calperiod[[i]]] * caldf$d13C_slope[i]

      co2_ambdf$mean_cal[var_inds_in_calperiod[[i]]] <- caldf$co2_intercept[i] +
        co2_ambdf$mean[var_inds_in_calperiod[[i]]] * caldf$co2_slope[i]

      co2_ambdf$min[var_inds_in_calperiod[[i]]]  <- caldf$co2_intercept[i] +
        co2_ambdf$min[var_inds_in_calperiod[[i]]] * caldf$co2_slope[i]

      co2_ambdf$max[var_inds_in_calperiod[[i]]]  <- caldf$co2_intercept[i] +
        co2_ambdf$max[var_inds_in_calperiod[[i]]] * caldf$co2_slope[i]

    }

    # round variance down to 2 digits
    d13C_ambdf$vari <- round(d13C_ambdf$vari, digits = 2)
    co2_ambdf$vari <- round(co2_ambdf$vari, digits = 2)

    # replace ambdf in amb.data.list, return amb.data.list
    amb_data_list$dlta13CCo2 <- d13C_ambdf
    amb_data_list$rtioMoleDryCo2 <- co2_ambdf

    # write out dataset to HDF5 file.
    fid <- rhdf5::H5Fopen(file)

    co2.data.outloc <- rhdf5::H5Gcreate(fid,
                            paste0("/", site, "/dp01/data/isoCo2/", outname))

    # loop through variables in amb.data.list and write out as a dataframe.
    lapply(names(amb_data_list), function(x) {
      rhdf5::h5writeDataset.data.frame(obj = amb_data_list[[x]],
                                h5loc = co2.data.outloc,
                                name = x,
                                DataFrameAsCompound = TRUE)})

    # close all open handles.
    rhdf5::h5closeAll()

}
