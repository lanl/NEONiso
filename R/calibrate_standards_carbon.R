#' calibrate_standards_carbon
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
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
  ref_df$dlta13CCo2$mean_cal <- ref_df$dlta13CCo2$mean
  ref_df$dlta13CCo2$mean_cal <- as.numeric(NA)

  ref_df$rtioMoleDryCo2$mean_cal <- ref_df$rtioMoleDryCo2$mean

  # convert start times to POSIXct.
  ref_df$dlta13CCo2$timeBgn <- as.POSIXct(ref_df$dlta13CCo2$timeBgn,
                                          format = "%Y-%m-%dT%H:%M:%OSZ",
                                          tz = "UTC")

  # check to see if we need to do corrections on bad reference gas values!
  if (correct_bad_refvals) {
    # ensure that a site and refGas were supplied.
    if (missing(site) | missing(refGas)) {
      stop("If trying to correct bad reference values, must supply site and refGas as arguments")
    }
    
    # okay, now apply corrections if necessary.
    if (site == "ONAQ") {
      if (refGas == "low") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                               ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                               ref_df$dlta13CCo2Refe$mean < -9.2] <- -9.054
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                   ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                   ref_df$rtioMoleDryCo2Refe$mean > 420] <- 366.94
      } else if (refGas == "med") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -9.2] <- -9.102
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 440] <- 412.10
      } else if (refGas == "high") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -13] <- -10.54
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 470] <- 466.643
      }
    } else if (site == "WOOD") {
      # okay, now apply corrections if necessary.
      if (refGas == "low") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -8.5 & ref_df$dlta13CCo2Refe$mean > -9] <- -9.267
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 363 & ref_df$rtioMoleDryCo2Refe$mean < 364] <- 366.56
      } else if (refGas == "med") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -10.4 & ref_df$dlta13CCo2Refe$mean > -10.6] <- -9.448
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 460 & ref_df$rtioMoleDryCo2Refe$mean < 461] <- 412.503
      } else if (refGas == "high") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -11.4 & ref_df$dlta13CCo2Refe$mean > -11.8] <- -10.392
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("06/18/2018", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("02/07/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 560 & ref_df$rtioMoleDryCo2Refe$mean < 570] <- 454.619

      }
    } else if (site == "BLAN") {
      # okay, now apply corrections if necessary.
      if (refGas == "low") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("04/01/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("08/22/2019", format = "%m/%d/%Y") &
                                     (is.na(ref_df$dlta13CCo2Refe$mean) | 
                                        ref_df$dlta13CCo2Refe$mean < -8.3 & ref_df$dlta13CCo2Refe$mean > -8.6)] <- -8.955
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("04/01/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("08/22/2019", format = "%m/%d/%Y") &
                                         (is.na(ref_df$rtioMoleDryCo2Refe$mean) | 
                                            ref_df$rtioMoleDryCo2Refe$mean > 360 & ref_df$rtioMoleDryCo2Refe$mean < 362)] <- 366.56
      } else if (refGas == "med") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("04/01/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("08/22/2019", format = "%m/%d/%Y") &
                                     (is.na(ref_df$dlta13CCo2Refe$mean) | 
                                        ref_df$dlta13CCo2Refe$mean < -9.4 & ref_df$dlta13CCo2Refe$mean > -9.6)] <- -9.448
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("04/01/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("08/22/2019", format = "%m/%d/%Y") &
                                         (is.na(ref_df$rtioMoleDryCo2Refe$mean) | 
                                            ref_df$rtioMoleDryCo2Refe$mean > 410 & ref_df$rtioMoleDryCo2Refe$mean < 415)] <- 435.80
      } else if (refGas == "high") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("04/01/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("08/22/2019", format = "%m/%d/%Y") &
                                     (is.na(ref_df$dlta13CCo2Refe$mean) | 
                                        ref_df$dlta13CCo2Refe$mean < -10.5 & ref_df$dlta13CCo2Refe$mean > -11.0)] <- -15.418
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("04/01/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("08/22/2019", format = "%m/%d/%Y") &
                                         (is.na(ref_df$rtioMoleDryCo2Refe$mean) | 
                                          ref_df$rtioMoleDryCo2Refe$mean > 505 & ref_df$rtioMoleDryCo2Refe$mean < 515)] <- 521.08
        
      }
    } else if (site == "STER") {
      if (refGas == "low") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("01/16/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("05/08/2019", format = "%m/%d/%Y") &
                                     (is.na(ref_df$dlta13CCo2Refe$mean) | 
                                        ref_df$dlta13CCo2Refe$mean < -8.4 & ref_df$dlta13CCo2Refe$mean > -8.6)] <- -8.763
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("01/16/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("05/08/2019", format = "%m/%d/%Y") &
                                         (is.na(ref_df$rtioMoleDryCo2Refe$mean) | 
                                            ref_df$rtioMoleDryCo2Refe$mean > 350 & ref_df$rtioMoleDryCo2Refe$mean < 352)] <- 360.047
      }

    } else if (site == "ORNL") {
      # okay, now apply corrections if necessary.
      if (refGas == "low") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("01/01/2020", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -8.5 & ref_df$dlta13CCo2Refe$mean > -8.7] <- -9.049
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("01/01/2020", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 350 & ref_df$rtioMoleDryCo2Refe$mean < 352] <- 367.82
      } else if (refGas == "med") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("11/14/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") & 
                                     (ref_df$dlta13CCo2Refe$mean < -10.6 | ref_df$dlta13CCo2Refe$mean > -10.5)] <- -10.575
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("11/14/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                         (ref_df$rtioMoleDryCo2Refe$mean > 450 | ref_df$rtioMoleDryCo2Refe$mean < 425)] <- 442.565
      } else if (refGas == "high") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("11/14/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean > -15.0] <- -15.575
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("11/14/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean < 530] <- 530.234
        
      }
    } else if (site == "TREE") {
      # okay, now apply corrections if necessary.
      if (refGas == "low") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("01/01/2020", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -8.3 & ref_df$dlta13CCo2Refe$mean > -8.7] <- -9.175
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("01/01/2020", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 345 & ref_df$rtioMoleDryCo2Refe$mean < 352] <- 368.58
      } else if (refGas == "med") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("01/01/2020", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") & 
                                     (ref_df$dlta13CCo2Refe$mean < -8 | ref_df$dlta13CCo2Refe$mean > -9)] <- -10.284
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("01/01/2020", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                         (ref_df$rtioMoleDryCo2Refe$mean > 380 | ref_df$rtioMoleDryCo2Refe$mean < 400)] <- 437.92
      } else if (refGas == "high") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("01/01/2020", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -15.0] <- -14.828
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("01/01/2020", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("05/31/2020", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 530] <- 511.21
        
      }
    } else if (site == "BARR") {
      # okay, now apply corrections if necessary.
      if (refGas == "low") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("06/01/2018", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("03/28/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -8.7 & ref_df$dlta13CCo2Refe$mean > -8.9] <- -8.607
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("06/01/2018", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("03/28/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 380 & ref_df$rtioMoleDryCo2Refe$mean < 390] <- 359.05
      } else if (refGas == "high") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("06/01/2018", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("03/28/2019", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$mean < -9.9 & ref_df$dlta13CCo2Refe$mean > -10] <- -11.501
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("06/01/2018", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("03/28/2019", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$mean > 420 & ref_df$rtioMoleDryCo2Refe$mean < 430] <- 555.26
        
      }
    } else if (site == "SRER") {
      if (refGas == "low") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("04/01/2018", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("08/01/2018", format = "%m/%d/%Y") &
                                     (ref_df$dlta13CCo2Refe$mean < -9 | is.na(ref_df$dlta13CCo2Refe$mean))] <- -8.702
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("04/01/2018", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("08/01/2018", format = "%m/%d/%Y") &
                                         (ref_df$rtioMoleDryCo2Refe$mean > 380 | is.na(ref_df$rtioMoleDryCo2Refe$mean))] <- 356.166
      } else if (refGas == "med") {
        ref_df$dlta13CCo2Refe$mean[ref_df$dlta13CCo2Refe$timeBgn > as.POSIXct("04/01/2018", format = "%m/%d/%Y") &
                                     ref_df$dlta13CCo2Refe$timeBgn < as.POSIXct("08/01/2018", format = "%m/%d/%Y") &
                                     (ref_df$dlta13CCo2Refe$mean < -14 | is.na(ref_df$dlta13CCo2Refe$mean))] <- -10.401
        ref_df$rtioMoleDryCo2Refe$mean[ref_df$rtioMoleDryCo2Refe$timeBgn > as.POSIXct("04/01/2018", format = "%m/%d/%Y") &
                                         ref_df$rtioMoleDryCo2Refe$timeBgn < as.POSIXct("08/01/2018", format = "%m/%d/%Y") &
                                         (ref_df$rtioMoleDryCo2Refe$mean > 485 | is.na(ref_df$rtioMoleDryCo2Refe$mean))] <- 428.784
        
      }
    }
  }
  
  # # check to see how many rows cal_df has
  # if ("gain12C" %in% names(cal_df)) {
  #   
  #   # check to ensure there are actual standard measurements for this month!
  #   if (nrow(ref_df$dlta13CCo2) > 1) {
  # 
  #     for (i in 2:nrow(ref_df$dlta13CCo2)) {
  #       # note: starts at 2 because periods are defined as
  #       # extending to the end time of standard measurement i
  #       # to the endtime of stanard measurement i + 1.
  # 
  #       # determine which row calibration point is in.
  #       int <- lubridate::interval(cal_df$start, cal_df$end)
  # 
  #       cal_id <- which(ref_df$dlta13CCo2$timeBgn[i] %within% int)
  # 
  #       # now, calculate calibrated value ONLY IF CERTAIN CONDITIONS ARE MET:
  #       # 1a-d. variables needed are not missing.
  #       # 2. at least 200 ~1Hz measurements available (e.g., valve issues)
  #       # 3. meets tolerance between ref and measured d13C
  #       # 4. meets tolerance between ref and measured CO2
  #       # 5. meets tolerance of d13C variance
  #       # these conditions are listed in this order in the logical below.
  #       if (!is.na(ref_df$dlta13CCo2$mean[i]) &
  #           !is.na(ref_df$dlta13CCo2Refe$mean[i]) &
  #           !is.na(ref_df$rtioMoleDryCo2$mean[i]) &
  #           !is.na(ref_df$rtioMoleDryCo2Refe$mean[i]) &
  #           ref_df$dlta13CCo2$numSamp[i] >= 200 &
  #           abs(ref_df$dlta13CCo2$mean[i] -
  #               ref_df$dlta13CCo2Refe$mean[i]) < 5 &
  #           abs(ref_df$rtioMoleDryCo2$mean[i] -
  #               ref_df$rtioMoleDryCo2Refe$mean[i]) < 10 &
  #           ref_df$dlta13CCo2$vari[i] < 5) {
  # 
  #         if (!(length(cal_id) == 0)) {
  #           # calibrate isotopologues using appropriate cal_id
  #           uncal_12C <- ref_df$rtioMoleDryCo2$mean[i] * (1 - f) /
  #             (1 + R_vpdb * (1 + ref_df$dlta13CCo2$mean[i] / 1000))
  # 
  #           uncal_13C <- ref_df$rtioMoleDryCo2$mean[i] * (1 - f) - uncal_12C
  # 
  #           cal_12C <- cal_df$gain12C[cal_id] * uncal_12C +
  #             cal_df$offset12C[cal_id]
  #           cal_13C <- cal_df$gain13C[cal_id] * uncal_13C +
  #             cal_df$offset13C[cal_id]
  # 
  #           if (!is.na(cal_df$r2_12C[cal_id]) &
  #               !is.na(cal_df$r2_13C[cal_id]) &
  #               cal_df$r2_12C[cal_id] > r2_thres &
  #               cal_df$r2_13C[cal_id] > r2_thres) {
  #             ref_df$dlta13CCo2$mean_cal[i] <- round(1000 * (cal_13C /
  #                                                    cal_12C / R_vpdb - 1), 3)
  #             ref_df$rtioMoleDryCo2$mean_cal[i] <- (cal_13C + cal_12C) /
  #                                                     (1 - f)
  # 
  #           } else {
  # 
  #             ref_df$dlta13CCo2$mean_cal[i] <- NA
  #             ref_df$rtioMoleDryCo2$mean_cal[i] <- NA
  # 
  #           }
  # 
  #         } else {
  # 
  #           ref_df$dlta13CCo2$mean_cal[i] <- NA
  #           ref_df$rtioMoleDryCo2$mean_cal[i] <- NA
  # 
  #         }
  # 
  #       } else {
  # 
  #         ref_df$dlta13CCo2$mean_cal[i] <- NA
  #         ref_df$rtioMoleDryCo2$mean_cal[i] <- NA
  #       }
  #     }
  #   }
  # 
  # } else {
  # 
  #   if (nrow(ref_df$dlta13CCo2) > 1) {
  #     for (i in 2:nrow(ref_df$dlta13CCo2)) { # use n-1 because of bracketing
  # 
  #       # determine which row calibration point is in.
  #       int <- lubridate::interval(cal_df$start, cal_df$end)
  #       cal_id <- which(ref_df$dlta13CCo2$timeBgn[i] %within% int)
  # 
  #       # now, calculate calibrated value ONLY IF CERTAIN CONDITIONS ARE MET:
  #       # 1a-d. variables needed are not missing.
  #       # 2. at least 200 ~1Hz measmnts available (filters out valve issues)
  #       # 3. meets tolerance between ref and measured d13C
  #       # 4. meets tolerance between ref and measured CO2
  #       # 5. meets tolerance of d13C variance
  #       # these conditions are listed in this order in the logical below.
  #       if (!is.na(ref_df$dlta13CCo2$mean[i]) &
  #           !is.na(ref_df$dlta13CCo2Refe$mean[i]) &
  #           !is.na(ref_df$rtioMoleDryCo2$mean[i]) &
  #           !is.na(ref_df$rtioMoleDryCo2Refe$mean[i]) &
  #           ref_df$dlta13CCo2$numSamp[i] >= 200 &
  #           abs(ref_df$dlta13CCo2$mean[i] -
  #               ref_df$dlta13CCo2Refe$mean[i]) < 5 &
  #           abs(ref_df$rtioMoleDryCo2$mean[i] -
  #               ref_df$rtioMoleDryCo2Refe$mean[i]) < 10 &
  #           ref_df$dlta13CCo2$vari[i] < 5) {
  # 
  #         if (!length(cal_id) == 0) {
  # 
  #           if (!is.na(cal_df$d13C_r2[cal_id]) &
  #               !is.na(cal_df$co2_r2[cal_id]) &
  #               cal_df$d13C_r2[cal_id] > r2_thres &
  #               cal_df$co2_r2[cal_id] > r2_thres) {
  # 
  #             ref_df$dlta13CCo2$mean_cal[i] <- cal_df$d13C_intercept[cal_id] +
  #               cal_df$d13C_slope[cal_id] * ref_df$dlta13CCo2$mean[i]
  # 
  #             ref_df$rtioMoleDryCo2$mean_cal[i] <- cal_df$co2_intercept[cal_id] +
  #               cal_df$co2_slope[cal_id] * ref_df$rtioMoleDryCo2$mean[i]
  # 
  #           } else {
  # 
  #             ref_df$dlta13CCo2$mean_cal[i] <- NA
  #             ref_df$rtioMoleDryCo2$mean_cal[i] <- NA
  #           }
  # 
  #         } else {
  # 
  #           ref_df$dlta13CCo2$mean_cal[i] <- NA
  #           ref_df$rtioMoleDryCo2$mean_cal[i] <- NA
  # 
  #         }
  # 
  #       } else {
  # 
  #         ref_df$dlta13CCo2$mean_cal[i] <- NA
  #         ref_df$rtioMoleDryCo2$mean_cal[i] <- NA
  # 
  #       }
  #     }
  #   }
  # }

  # convert time back to NEON format.
  ref_df$dlta13CCo2$timeBgn <- convert_POSIXct_to_NEONhdf5_time(ref_df$dlta13CCo2$timeBgn)

  # return ref_data_frame
  return(ref_df)

}
