#' calibrate_standards_carbon
#'
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param cal_df Data.frame containing calibration parameters
#' @param ref_df Data.frame containing reference gas measurements
#' @param f Fraction of CO2 isotopologues that are not 12CO2 or 13CO2.
#'          Inherited from script calling this function.
#' @param r2_thres Threshold for calibration regression to be used to
#'          calibrate standards data. Default is 0.95. Calibrated reference
#'          gas measurements occurring during calibration periods
#'          with r2 values less than `r2_thres` will be marked NA.
#' @param correct_bad_refvals Should we correct known/suspected incorrect
#'          reference values in the NEON HDF5 files? (Default = `FALSE`).
#' @param site Four letter NEON site code.
#'             Only used if `correct_bad_refvals = TRUE`.
#' @param refGas One of "low", "med", or "high."
#'             Only used if `correct_bad_refvals = TRUE`.
#'
#' @return A data.frame having the same number of rows of `cal_df`, with
#'        additional columns added for calibrated CO2 mole fractions
#'        and d13C values.
calibrate_standards_carbon <- function(cal_df,
                                       ref_df,
                                       f = 0.00474,
                                       r2_thres = 0.95,
                                       correct_bad_refvals = FALSE,
                                       site,
                                       refGas) {

  # get R_vpdb
  R_vpdb <- get_Rstd("carbon")

  # want to implement same tolerances used to generate calibration regression!
  # need to assess the CO2 and d13C tolerances wrt reference values.

  # calibrate standards using value for corresponding calibration period.
  ref_df$dlta13CCo2$mean_cal <- rep(NA, length(ref_df$dlta13CCo2$mean))
  ref_df$rtioMoleDryCo2$mean_cal <- rep(NA, length(ref_df$rtioMoleDryCo2$mean))

  # convert start times to POSIXct.
  ref_df$dlta13CCo2$timeBgn <- as.POSIXct(ref_df$dlta13CCo2$timeBgn,
                                          format = "%Y-%m-%dT%H:%M:%OSZ",
                                          tz = "UTC")

  # check to see how many rows cal_df has
  if ("gain12C" %in% names(cal_df)) {

    # check to ensure there are actual standard measurements for this month!
    if (nrow(ref_df$dlta13CCo2) > 1) {

      for (i in 2:nrow(ref_df$dlta13CCo2)) {
        # note: starts at 2 because periods are defined as
        # extending to the end time of standard measurement i
        # to the endtime of standard measurement i + 1.

        # determine which row calibration point is in.
        int <- lubridate::interval(cal_df$timeBgn, cal_df$timeEnd)

        cal_id <- which(ref_df$dlta13CCo2$timeBgn[i] %within% int)

        # now, calculate calibrated value ONLY IF CERTAIN CONDITIONS ARE MET:
        # 1a-d. variables needed are not missing.
        # 2. at least 200 ~1Hz measurements available (e.g., valve issues)
        # 3. meets tolerance between ref and measured d13C
        # 4. meets tolerance between ref and measured CO2
        # 5. meets tolerance of d13C variance
        # these conditions are listed in this order in the logical below.
        if (!is.na(ref_df$dlta13CCo2$mean[i]) &
            !is.na(ref_df$dlta13CCo2Refe$mean[i]) &
            !is.na(ref_df$rtioMoleDryCo2$mean[i]) &
            !is.na(ref_df$rtioMoleDryCo2Refe$mean[i]) &
            ref_df$dlta13CCo2$numSamp[i] >= 200 &
            abs(ref_df$dlta13CCo2$mean[i] -
                ref_df$dlta13CCo2Refe$mean[i]) < 5 &
            abs(ref_df$rtioMoleDryCo2$mean[i] -
                ref_df$rtioMoleDryCo2Refe$mean[i]) < 10 &
            ref_df$dlta13CCo2$vari[i] < 5) {

          if (!(length(cal_id) == 0)) {
            # calibrate isotopologues using appropriate cal_id
            uncal_12C <- calculate_12CO2(ref_df$rtioMoleDryCo2$mean[i],
                                         ref_df$dlta13CCo2$mean[i])

            uncal_13C <- calculate_13CO2(ref_df$rtioMoleDryCo2$mean[i],
                                         ref_df$dlta13CCo2$mean[i])

            cal_12C <- cal_df$gain12C[cal_id] * uncal_12C +
              cal_df$offset12C[cal_id]
            cal_13C <- cal_df$gain13C[cal_id] * uncal_13C +
              cal_df$offset13C[cal_id]

            if (!is.na(cal_df$r2_12C[cal_id]) &
                !is.na(cal_df$r2_13C[cal_id]) &
                cal_df$r2_12C[cal_id] > r2_thres &
                cal_df$r2_13C[cal_id] > r2_thres) {
              ref_df$dlta13CCo2$mean_cal[i] <- R_to_delta(cal_13C / cal_12C,
                                                          "carbon")
              ref_df$rtioMoleDryCo2$mean_cal[i] <- (cal_13C + cal_12C) /
                                                      (1 - f)

            } else {

              ref_df$dlta13CCo2$mean_cal[i] <- NA
              ref_df$rtioMoleDryCo2$mean_cal[i] <- NA

            }

          } else {

            ref_df$dlta13CCo2$mean_cal[i] <- NA
            ref_df$rtioMoleDryCo2$mean_cal[i] <- NA

          }

        } else {

          ref_df$dlta13CCo2$mean_cal[i] <- NA
          ref_df$rtioMoleDryCo2$mean_cal[i] <- NA
        }
      }
    }

  } else {

    if (nrow(ref_df$dlta13CCo2) > 1) {
      for (i in 2:nrow(ref_df$dlta13CCo2)) { # use n-1 because of bracketing

        # determine which row calibration point is in.
        int <- lubridate::interval(cal_df$timeBgn, cal_df$timeEnd)
        cal_id <- which(ref_df$dlta13CCo2$timeBgn[i] %within% int)

        # now, calculate calibrated value ONLY IF CERTAIN CONDITIONS ARE MET:
        # 1a-d. variables needed are not missing.
        # 2. at least 200 ~1Hz measmnts available (filters out valve issues)
        # 3. meets tolerance between ref and measured d13C
        # 4. meets tolerance between ref and measured CO2
        # 5. meets tolerance of d13C variance
        # these conditions are listed in this order in the logical below.
        if (!is.na(ref_df$dlta13CCo2$mean[i]) &
            !is.na(ref_df$dlta13CCo2Refe$mean[i]) &
            !is.na(ref_df$rtioMoleDryCo2$mean[i]) &
            !is.na(ref_df$rtioMoleDryCo2Refe$mean[i]) &
            ref_df$dlta13CCo2$numSamp[i] >= 200 &
            abs(ref_df$dlta13CCo2$mean[i] -
                ref_df$dlta13CCo2Refe$mean[i]) < 5 &
            abs(ref_df$rtioMoleDryCo2$mean[i] -
                ref_df$rtioMoleDryCo2Refe$mean[i]) < 10 &
            ref_df$dlta13CCo2$vari[i] < 5) {

          if (!length(cal_id) == 0) {

            if (!is.na(cal_df$d13C_r2[cal_id]) &
                !is.na(cal_df$co2_r2[cal_id]) &
                cal_df$d13C_r2[cal_id] > r2_thres &
                cal_df$co2_r2[cal_id] > r2_thres) {

              ref_df$dlta13CCo2$mean_cal[i] <- cal_df$d13C_intercept[cal_id] +
                cal_df$d13C_slope[cal_id] * ref_df$dlta13CCo2$mean[i]

              ref_df$rtioMoleDryCo2$mean_cal[i] <- cal_df$co2_intercept[cal_id] +
                cal_df$co2_slope[cal_id] * ref_df$rtioMoleDryCo2$mean[i]

            } else {

              ref_df$dlta13CCo2$mean_cal[i] <- NA
              ref_df$rtioMoleDryCo2$mean_cal[i] <- NA
            }

          } else {

            ref_df$dlta13CCo2$mean_cal[i] <- NA
            ref_df$rtioMoleDryCo2$mean_cal[i] <- NA

          }

        } else {

          ref_df$dlta13CCo2$mean_cal[i] <- NA
          ref_df$rtioMoleDryCo2$mean_cal[i] <- NA

        }
      }
    }
  }

  # convert time back to NEON format.
  ref_df$dlta13CCo2$timeBgn <- convert_POSIXct_to_NEONhdf5_time(ref_df$dlta13CCo2$timeBgn)

  # return ref_data_frame
  return(ref_df)

}
