#' calibrate_standards_water
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param cal_df Data.frame containing calibration parameters
#' @param ref_df Data.frame containing reference gas measurements
#' @param r2_thres Threshold for calibration regression to be used to
#'          calibrate standards data. Default is 0.95. Calibrated reference
#'          gas measurements occurring during calibration periods
#'          with r2 values less than `r2_thres` will be marked NA.
#' @export
calibrate_standards_water <- function(cal_df,
                                      ref_df,
                                      r2_thres = 0.95) {

  # want to implement same tolerances used to generate calibration regression!
  # need to assess the CO2 and d13C tolerances wrt reference values.

  # calibrate standards using value for corresponding calibration period.
  ref_df$dlta18OH2o$mean_cal <- ref_df$dlta18OH2o$mean
  ref_df$dlta18OH2o$mean_cal <- as.numeric(NA)

  ref_df$dlta2HH2o$mean_cal <- ref_df$dlta2HH2o$mean
  ref_df$dlta2HH2o$mean_cal <- as.numeric(NA)

  # convert start times to POSIXct.
  ref_df$dlta18OH2o$timeBgn <- convert_NEONhdf5_to_POSIXct_time(ref_df$dlta18OH2o$timeBgn)
  ref_df$dlta2HH2o$timeBgn <- convert_NEONhdf5_to_POSIXct_time(ref_df$dlta2HH2o$timeBgn)

  # Calibrate oxygen isotope ratios.
  if (nrow(ref_df$dlta18OH2o) > 1) {
    for (i in 1:nrow(ref_df$dlta18OH2o)) {

      # determine which row calibration point is in.
      int <- lubridate::interval(cal_df$start, cal_df$end)
      cal_id <- which(ref_df$dlta18OH2o$timeBgn[i] %within% int)

      # now, calculate calibrated value ONLY IF CERTAIN CONDITIONS ARE MET:
      # 1a-d. variables needed are not missing.
      # 2. at least 200 ~1Hz measmnts available (filters out valve issues)
      # 3. meets tolerance between ref and measured d13C
      # 4. meets tolerance between ref and measured CO2
      # 5. meets tolerance of d13C variance
      # these conditions are listed in this order in the logical below.
      if (!is.na(ref_df$dlta18OH2o$mean[i]) &
          !is.na(ref_df$dlta18OH2oRefe$mean[i]) &
          ref_df$dlta18OH2o$numSamp[i] >= 30 &
          abs(ref_df$dlta18OH2o$mean[i] -
              ref_df$dlta18OH2oRefe$mean[i]) < 10 &
          ref_df$dlta18OH2o$vari[i] < 5) {

        if (!length(cal_id) == 0) {

          if (!is.na(cal_df$o_r2[cal_id]) &
              cal_df$o_r2[cal_id] > r2_thres) {

            ref_df$dlta18OH2o$mean_cal[i] <- cal_df$o_intercept[cal_id] +
              cal_df$o_slope[cal_id] * ref_df$dlta18OH2o$mean[i]

          } else {

            ref_df$dlta18OH2o$mean_cal[i] <- NA
          }

        } else {

          ref_df$dlta18OH2o$mean_cal[i] <- NA

        }

      } else {

        ref_df$dlta18OH2o$mean_cal[i] <- NA

      }
    }
  }

  # Calibrate hydrogen isotope ratios.
  if (nrow(ref_df$dlta2HH2o) > 1) {
    for (i in 1:nrow(ref_df$dlta2HH2o)) {

      # determine which row calibration point is in.
      int <- lubridate::interval(cal_df$start, cal_df$end)
      cal_id <- which(ref_df$dlta2HH2o$timeBgn[i] %within% int)

      # now, calculate calibrated value ONLY IF CERTAIN CONDITIONS ARE MET:
      # 1a-d. variables needed are not missing.
      # 2. at least 200 ~1Hz measmnts available (filters out valve issues)
      # 3. meets tolerance between ref and measured d13C
      # 4. meets tolerance between ref and measured CO2
      # 5. meets tolerance of d13C variance
      # these conditions are listed in this order in the logical below.
      if (!is.na(ref_df$dlta2HH2o$mean[i]) &
          !is.na(ref_df$dlta2HH2oRefe$mean[i]) &
          ref_df$dlta2HH2o$numSamp[i] >= 30 &
          abs(ref_df$dlta2HH2o$mean[i] -
              ref_df$dlta2HH2oRefe$mean[i]) < 40 &
          ref_df$dlta2HH2o$vari[i] < 20) {

        if (!length(cal_id) == 0) {

          if (!is.na(cal_df$h_r2[cal_id]) &
              cal_df$h_r2[cal_id] > r2_thres) {

            ref_df$dlta2HH2o$mean_cal[i] <- cal_df$h_intercept[cal_id] +
              cal_df$h_slope[cal_id] * ref_df$dlta2HH2o$mean[i]

          } else {

            ref_df$dlta2HH2o$mean_cal[i] <- NA
          }

        } else {

          ref_df$dlta2HH2o$mean_cal[i] <- NA

        }

      } else {

        ref_df$dlta2HH2o$mean_cal[i] <- NA

      }
    }
  }

  # convert time back to NEON format.
  ref_df$dlta18OH2o$timeBgn <- convert_POSIXct_to_NEONhdf5_time(ref_df$dlta18OH2o$timeBgn)
  ref_df$dlta2HH2o$timeBgn <- convert_POSIXct_to_NEONhdf5_time(ref_df$dlta2HH2o$timeBgn)

  # return ref_data_frame
  return(ref_df)

}
