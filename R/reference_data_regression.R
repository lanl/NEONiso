#' loocv
#' 
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#' 
#' helper function for the leave-one-out cross variance
#'
#' @param mod Fitted model to estimate leave-one-out CV on.
#'
loocv <- function(mod) {
  h = stats::lm.influence(mod)$hat
  return(base::mean((stats::residuals(mod)/(1-h))^2)) # might need to add na.rm
}

#' estimate_calibration_error
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param data Data frame to perform cross-validation on.
#' @param formula Formula to pass to caret::train to perform cross validation.
#'
estimate_calibration_error <- function(formula, data) {
  
  # force data to be a dataframe, as caret chokes on tibbles evidently.
  data2 <- as.data.frame(data)
  
  ctrl <- caret::trainControl(method = 'cv', number = 5)
  
  model <- suppressWarnings(caret::train(form = formula, data = data2,
                        method = 'lm',
                        trControl = ctrl,
                        na.action = stats::na.omit))
  
  output <- model$results[c("RMSE","Rsquared","MAE")]
  
  return(output)
  
}

#' fit_carbon_regression
#' 
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param method Are we using the Bowling et al. 2003 method
#'              ("Bowling_2003") or direct linear regression of
#'              d13C and CO2 mole fractions ("linreg")?
#' @param calibration_half_width Determines the period (in days)
#'        from which reference data are selected (period
#'        is 2*calibration_half_width).
#' @param ref_data Reference data.frame from which to estimate 
#'        calibration parameters.
#' @param plot_regression_data True or false - should we plot the data used in 
#'        the regression? Useful for debugging.
#' @param plot_dir If plot_regression_data is true, where should the 
#'        plots be saved?
#' @param site Needed for regression plots.
#'
#' @return Returns a data.frame of calibration parameters. If
#'        `method == "Bowling_2003"`, then data.frame includes 
#'        gain and offset parameters for 12CO2 and 13CO2, and r^2
#'        values for each regression. If `method == "linreg"`,
#'        then data.frame includes slope, intercept, and r^2 values
#'        for d13C and CO2 values.
#'
#' @importFrom stats coef lm

fit_carbon_regression <- function(ref_data, method, calibration_half_width,
                                  plot_regression_data = FALSE,
                                  plot_dir ='/dev/null',
                                  site) {
  
  # First, identify year/month combination and then select data.
  yrmn <- paste(lubridate::year(ref_data$timeBgn)[1],
                lubridate::month(ref_data$timeBgn)[1],
                sep = "-")
  
  # validate yrmn - is it actually a date? if no refdata, then no.
  #---------------------------------------------------------------
  # Select which validation data to carry through to calibration
  #---------------------------------------------------------------
  ref_data <- select_daily_reference_data(ref_data, analyte = 'co2')
  
  if (method == "Bowling_2003") {
    
    # calculate mole fraction (12CO2 / 13CO2) for ref gases and observed values
    ref_data$conc12CCO2_ref = calculate_12CO2(ref_data$rtioMoleDryCo2Refe.mean, ref_data$dlta13CCo2Refe.mean)
    ref_data$conc13CCO2_ref = calculate_13CO2(ref_data$rtioMoleDryCo2Refe.mean, ref_data$dlta13CCo2Refe.mean)
    ref_data$conc12CCO2_obs = calculate_12CO2(ref_data$rtioMoleDryCo2.mean, ref_data$dlta13CCo2.mean)
    ref_data$conc13CCO2_obs = calculate_13CO2(ref_data$rtioMoleDryCo2.mean, ref_data$dlta13CCo2.mean)
    
    if (nrow(ref_data) == 0) {
      
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out <- data.frame(timeBgn = as.POSIXct(yrmn,
                                           format = "%Y-%m",
                                           tz = "UTC",
                                           origin = "1970-01-01"),
                        timeEnd = as.POSIXct(yrmn,
                                         format = "%Y-%m",
                                         tz = "UTC",
                                         origin = "1970-01-01"),
                        gain12C = as.numeric(NA),
                        gain13C = as.numeric(NA),
                        offset12C = as.numeric(NA),
                        offset13C = as.numeric(NA),
                        r2_12C = as.numeric(NA),
                        r2_13C = as.numeric(NA),
                        cvloo_12C = as.numeric(NA),
                        cvloo_13C = as.numeric(NA),
                        cv5mae_12C = as.numeric(NA),
                        cv5mae_13C = as.numeric(NA),
                        cv5rmse_12C = as.numeric(NA),
                        cv5rmse_13C = as.numeric(NA))
    } else {
      
      out <- data.frame(gain12C   = numeric(length = 2e5),
                        gain13C   = numeric(length = 2e5),
                        offset12C = numeric(length = 2e5),
                        offset13C = numeric(length = 2e5),
                        r2_12C    = numeric(length = 2e5),
                        r2_13C    = numeric(length = 2e5),
                        cvloo_12C = numeric(length = 2e5),
                        cvloo_13C = numeric(length = 2e5),
                        cv5mae_12C = numeric(length = 2e5),
                        cv5mae_13C = numeric(length = 2e5),
                        cv5rmse_12C = numeric(length = 2e5),
                        cv5rmse_13C = numeric(length = 2e5))
      
      # get start and end days.
      start_date <- as.Date(min(ref_data$timeBgn))
      end_date   <- as.Date(max(ref_data$timeEnd))
      
      # generate date sequence
      date_seq <- base::seq.Date(start_date, end_date, by = "1 day")
      
      # initialize vectors to hold times.
      start_time <- end_time <- vector()
      
      # okay, now run calibrations...
      
      for (i in 1:length(date_seq)) {
        start_time[i] <- as.POSIXct(paste(date_seq[i],"00:00:00.0001"), # odd workaround to prevent R from converting this to NA.
                                    tz = "UTC", origin = "1970-01-01")
        end_time[i]   <- as.POSIXct(paste(date_seq[i],"23:59:59.0000"),
                                    tz = "UTC", origin = "1970-01-01")
        
        # define calibration interval
        cal_period <- lubridate::interval(date_seq[i] - lubridate::ddays(calibration_half_width),
                                          date_seq[i] + lubridate::ddays(calibration_half_width))
        
        # select data subset using the interval
        cal_subset <- ref_data %>%
          dplyr::filter(.data$timeBgn %within% cal_period)
        
        if (plot_regression_data) {
          print("Plotting regression data...this step will take some time...")
          carbon_regression_plots(cal_subset,
                                  plot_filename = paste0(plot_dir, "/", site, "_", date_seq[i],".pdf"),
                                  method = method,
                                  mtitle = paste0(site, date_seq[i]))
        }
        #---------------------------------------------
        # do some light validation of these points.
        cal_subset <- cal_subset %>%
          dplyr::filter(.data$dlta13CCo2.vari < 5 &
                          abs(.data$rtioMoleDryCo2.mean - .data$rtioMoleDryCo2Refe.mean) < 10 &
                          abs(.data$dlta13CCo2.mean - .data$dlta13CCo2Refe.mean) < 5)
        
        if (length(unique(cal_subset$verticalPosition)) >= 2 & # >= 2 standards
            !all(is.na(cal_subset$dlta13CCo2.mean)) & # not all obs missing
            !all(is.na(cal_subset$dlta13CCo2Refe.mean))) { # not all ref missing
          
          tmpmod12C <- stats::lm(conc12CCO2_ref ~ conc12CCO2_obs, data = cal_subset)
          tmpmod13C <- stats::lm(conc13CCO2_ref ~ conc13CCO2_obs, data = cal_subset)
          
          # calculate gain and offset values.
          out$gain12C[i]   <- stats::coef(tmpmod12C)[[2]]
          out$gain13C[i]   <- stats::coef(tmpmod13C)[[2]]
          out$offset12C[i] <- stats::coef(tmpmod12C)[[1]]
          out$offset13C[i] <- stats::coef(tmpmod13C)[[1]]
          
          # extract r2
          out$r2_12C[i] <- summary(tmpmod12C)$r.squared
          out$r2_13C[i] <- summary(tmpmod13C)$r.squared
          
          # extract leave-one-out CV value
          out$cvloo_12C[i] <- loocv(tmpmod12C)
          out$cvloo_13C[i] <- loocv(tmpmod13C)
          
          # get cv5 values  
          tmp <- stats::formula(conc12CCO2_ref ~ conc12CCO2_obs)
          cv12C <- estimate_calibration_error(tmp, cal_subset)
          tmp <- stats::formula(conc13CCO2_ref ~ conc13CCO2_obs)
          cv13C <- estimate_calibration_error(tmp, cal_subset)
          
          # assign cv values:
          out$cv5mae_12C[i] <- cv12C$MAE
          out$cv5mae_13C[i] <- cv13C$MAE
          out$cv5rmse_12C[i] <- cv12C$RMSE
          out$cv5rmse_13C[i] <- cv13C$RMSE
          
        } else {
      
          out$gain12C[i]      <- NA
          out$gain13C[i]      <- NA
          out$offset12C[i]    <- NA
          out$offset13C[i]    <- NA
          out$r2_12C[i]       <- NA
          out$r2_13C[i]       <- NA
          out$cvloo_12C[i]    <- NA
          out$cvloo_13C[i]    <- NA          
          out$cv5mae_12C[i]   <- NA
          out$cv5mae_13C[i]   <- NA
          out$cv5rmse_12C[i]  <- NA
          out$cv5rmse_13C[i]  <- NA
        }
      }

      #subset out data frame.
      out <- out[1:length(start_time),]
      
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out$timeBgn <- as.POSIXct(start_time, tz = "UTC", origin = "1970-01-01")
      out$timeEnd <- as.POSIXct(end_time, tz = "UTC", origin = "1970-01-01")
      
      # re-order columns to ensure that they are consistent across methods
      out <- out[, c("timeBgn", "timeEnd",
                     "gain12C", "offset12C", "r2_12C", 
                     "cvloo_12C", "cv5mae_12C", "cv5rmse_12C",
                     "gain13C", "offset13C", "r2_13C",
                     "cvloo_13C", "cv5mae_13C", "cv5rmse_13C")]
      
    }
  } else if (method == "linreg") {
    
    if (nrow(ref_data) == 0) {
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out <- data.frame(timeBgn = as.POSIXct(yrmn,
                                           format = "%Y-%m",
                                           tz = "UTC",
                                           origin = "1970-01-01"),
                        timeEnd = as.POSIXct(yrmn,
                                         format = "%Y-%m",
                                         tz = "UTC",
                                         origin = "1970-01-01"),
                        d13C_slope     = as.numeric(NA),
                        d13C_intercept = as.numeric(NA),
                        d13C_r2        = as.numeric(NA),
                        d13C_cvloo     = as.numeric(NA),
                        d13C_cv5mae    = as.numeric(NA),
                        d13C_cv5rmse   = as.numeric(NA),
                        co2_slope      = as.numeric(NA),
                        co2_intercept  = as.numeric(NA),
                        co2_r2         = as.numeric(NA),
                        co2_cvloo      = as.numeric(NA),
                        co2_cv5mae     = as.numeric(NA),
                        co2_cv5rmse    = as.numeric(NA))
    } else {
      
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out <- data.frame(d13C_slope     = numeric(length = 2e5),
                        d13C_intercept = numeric(length = 2e5),
                        d13C_r2        = numeric(length = 2e5),
                        d13C_cvloo     = numeric(length = 2e5),
                        d13C_cv5mae    = numeric(length = 2e5),
                        d13C_cv5rmse   = numeric(length = 2e5),
                        co2_slope      = numeric(length = 2e5),
                        co2_intercept  = numeric(length = 2e5),
                        co2_r2         = numeric(length = 2e5),
                        co2_cvloo      = numeric(length = 2e5),
                        co2_cv5mae     = numeric(length = 2e5),
                        co2_cv5rmse    = numeric(length = 2e5))
      
      # get start and end days.
      start_date <- as.Date(min(ref_data$timeBgn))
      end_date   <- as.Date(max(ref_data$timeEnd))
      
      # generate date sequence
      date_seq <- base::seq.Date(start_date, end_date, by = "1 day")
      
      # initialize vectors to hold times.
      start_time <- end_time <- vector()
      
      # okay, now run calibrations...
      for (i in 1:length(date_seq)) {
        
        start_time[i] <- as.POSIXct(paste(date_seq[i],"00:00:00"),
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "UTC", origin = "1970-01-01")
        end_time[i]   <- as.POSIXct(paste(date_seq[i],"23:59:59"),
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "UTC", origin = "1970-01-01")
        
        # define calibration interval
        cal_period <- lubridate::interval(date_seq[i] - lubridate::ddays(calibration_half_width),
                                          date_seq[i] + lubridate::ddays(calibration_half_width))
        
        # select data subset using the interval
        cal_subset <- ref_data %>%
          dplyr::filter(.data$timeBgn %within% cal_period)
        
        #---------------------------------------------
        # do some light validation of these points.
        cal_subset <- cal_subset %>%
          dplyr::filter(.data$dlta13CCo2.vari < 5 &
                          abs(.data$rtioMoleDryCo2.mean - .data$rtioMoleDryCo2Refe.mean) < 10 &
                          abs(.data$dlta13CCo2.mean - .data$dlta13CCo2Refe.mean) < 5)
        
        if (length(unique(cal_subset$verticalPosition)) >= 2 & # >= 2 standards
            !all(is.na(cal_subset$dlta13CCo2.mean)) & # not all obs missing
            !all(is.na(cal_subset$dlta13CCo2Refe.mean))) { # not all ref missing
          
          # model to calibrate delta 13C values.
          tmpmod_d13 <- stats::lm(dlta13CCo2Refe.mean ~ dlta13CCo2.mean, data = cal_subset)
          tmpmod_co2 <- stats::lm(rtioMoleDryCo2Refe.mean ~ rtioMoleDryCo2.mean,
                                  data = cal_subset)
          
          
          out$d13C_slope[i]     <- coef(tmpmod_d13)[[2]]
          out$d13C_intercept[i] <- coef(tmpmod_d13)[[1]]
          out$d13C_r2[i]        <- summary(tmpmod_d13)$r.squared
          
          out$co2_slope[i]      <- coef(tmpmod_co2)[[2]]
          out$co2_intercept[i]  <- coef(tmpmod_co2)[[1]]
          out$co2_r2[i]         <- summary(tmpmod_co2)$r.squared
          
          # extract uncertainties:
          # extract leave-one-out CV value
          out$d13C_cvloo[i] <- loocv(tmpmod_d13)
          out$co2_cvloo[i]  <- loocv(tmpmod_co2)
          
          # get cv5 values  
          tmp <- stats::formula(dlta13CCo2Refe.mean ~ dlta13CCo2.mean)
          cv_d13C <- estimate_calibration_error(tmp, cal_subset)
          tmp <- stats::formula(rtioMoleDryCo2Refe.mean ~ rtioMoleDryCo2.mean)
          cv_co2 <- estimate_calibration_error(tmp, cal_subset)
          
          # assign cv values:
          out$d13C_cv5mae[i]  <- cv_d13C$MAE
          out$co2_cv5mae[i]   <- cv_co2$MAE
          out$d13C_cv5rmse[i] <- cv_d13C$RMSE
          out$co2_cv5rmse[i]  <- cv_co2$RMSE
          
        } else {
          
          out$d13C_slope[i]     <- NA
          out$d13C_intercept[i] <- NA
          out$d13C_r2[i]        <- NA
          
          out$co2_slope[i]      <- NA
          out$co2_intercept[i]  <- NA
          out$co2_r2[i]         <- NA
          
          # set uncertainty values to NA as well.
          out$d13C_cvloo[i]   <- NA
          out$d13C_cv5mae[i]  <- NA
          out$d13C_cv5rmse[i] <- NA
          out$co2_cvloo[i]    <- NA
          out$co2_cv5mae[i]   <- NA
          out$co2_cv5rmse[i]  <- NA
          
        }
      }

      #subset out data frame.
      out <- out[1:length(start_time),]
      
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out$timeBgn <- as.POSIXct(start_time, tz = "UTC", origin = "1970-01-01")
      out$timeEnd <- as.POSIXct(end_time, tz = "UTC", origin = "1970-01-01")

      # re-order columns to ensure that they are consistent across methods
      out <- out[, c("timeBgn", "timeEnd",
                     "d13C_slope", "d13C_intercept", "d13C_r2",
                     "d13C_cvloo", "d13C_cv5mae", "d13C_cv5rmse",
                     "co2_slope", "co2_intercept", "co2_r2",
                     "co2_cvloo", "co2_cv5mae", "co2_cv5rmse")]
      
    }
  }
   
  
  # ensure that not all dates are missing...narrowly defined here to 
  # be if out only has 1 row (should be the only case where this happens!)
  if (nrow(out) == 1) {
    if (is.na(out$timeBgn)) {
      out$timeBgn <- as.POSIXct("2010-01-01",format = "%Y-%m-%d")
      out$timeEnd <- as.POSIXct("2010-01-01",format = "%Y-%m-%d")
    }
  }

  # check to see if any out$timeBgn or end are NA
  if (sum(is.na(out$timeBgn)) > 0 | sum(is.na(out$timeEnd)) > 0) {
    stop("NA in calibration data frame time - how did I get here?")
  }
    
  return(out)
    
}
 
#' fit_water_regression
#'
#' @param stds Reference data.frame from which to estimate 
#'        calibration parameters.
#' @param calibration_half_width Determines the period (in days)
#'        from which reference data are selected (period
#'        is 2*calibration_half_width).
#' @param slope_tolerance Allows for filtering of slopes that deviate
#'        from 1 by slope_tolerance.
#' @param r2_thres What is the minimum r2 value permitted in a 'useful'
#'        calibration relationship.
#' 
#' @return Returns a data.frame of calibration parameters.
#'        Output data.frame includes slope, intercept, and r^2 values
#'        for d13C and CO2 values.
#'
#' @importFrom stats coef lm
#' 
fit_water_regression <- function(stds, calibration_half_width, slope_tolerance, r2_thres) {
  # do some light validation of these points.
  stds <- stds %>%
    dplyr::filter(.data$d18O_meas_var < 5 &
                    abs(.data$d18O_meas_mean - .data$d18O_ref_mean) < 3 &
                    .data$d2H_meas_var < 10 &
                    abs(.data$d18O_meas_mean - .data$d18O_ref_mean) < 24)
  
  
  if (nrow(stds) > 0) {
    # replace NaNs with NA
    # is.na() also returns NaN as NA, so this does actually do what first
    # comment indicates.
    stds[is.na(stds)] <- NA
    
    #-----------------------------------------------------------
    # CALIBRATE WATER ISOTOPE VALUES
    
    # reorder data frame
    stds <- stds[order(stds$btime), ]
    
    # loop through days represented in the dataframe,
    # select rows corresponding to window of interest, 
    # and run regression.
    
    # get start and end days.
    start_date <- as.Date(min(stds$btime))
    end_date   <- as.Date(max(stds$etime))
    
    # set up output vectors
    oxy_cal_slopes <- vector()
    oxy_cal_ints   <- vector()
    oxy_cal_rsq    <- vector()
    
    hyd_cal_slopes <- vector()
    hyd_cal_ints   <- vector()
    hyd_cal_rsq    <- vector()
    
    start_time     <- vector()
    end_time       <- vector()
    
    # loop through days and get regression statistics.
    # generate sequence of dates:
    loop_start_date <- start_date #+ lubridate::days(x = calibration_half_width)
    loop_end_date   <- end_date #- lubridate::days(x = calibration_half_width)
    
    # generate date sequence
    date_seq <- base::seq.Date(loop_start_date, loop_end_date, by = "1 day")
    
    for (i in 1:length(date_seq)) {
      
      start_time[i] <- as.POSIXct(paste(date_seq[i],"00:00:00.0001"), # odd workaround to prevent R from converting this to NA.
                                  tz = "UTC", origin = "1970-01-01")
      end_time[i]   <- as.POSIXct(paste(date_seq[i],"23:59:59.0000"),
                                  tz = "UTC", origin = "1970-01-01")
      
      # define calibration interval
      cal_period <- lubridate::interval(date_seq[i] - lubridate::days(calibration_half_width),
                                        date_seq[i] + lubridate::days(calibration_half_width))
      
      # select data subset using the interval
      std_subset <- stds %>%
        dplyr::filter(.data$btime %within% cal_period)
      
      # check to see if sum of is.na() on oxygen data = nrow of oxygen data
      if (sum(is.na(std_subset$d18O_meas_mean)) < nrow(std_subset) &
          sum(is.na(std_subset$d18O_ref_mean)) < nrow(std_subset)) {
        tmp <- lm(d18O_ref_mean ~ d18O_meas_mean, data = std_subset)
        
        oxy_cal_slopes[i] <- coef(tmp)[[2]]
        oxy_cal_ints[i]   <- coef(tmp)[[1]]
        oxy_cal_rsq[i]    <- summary(tmp)$r.squared
        
        # enforce thresholds. replace regression parameters as NA where they fail.
        if (!is.na(oxy_cal_rsq[i])) {
          if ((oxy_cal_slopes[i] > (1 + slope_tolerance)) |
              (oxy_cal_slopes[i] < (1 - slope_tolerance)) |
              (oxy_cal_rsq[i] < r2_thres)) {
            
            # set as NA
            oxy_cal_slopes[i] <- NA
            oxy_cal_ints[i]   <- NA
            oxy_cal_rsq[i]    <- NA
          }
        } else {
          # set as NA
          oxy_cal_slopes[i] <- NA
          oxy_cal_ints[i]   <- NA
          oxy_cal_rsq[i]    <- NA
        }
        
      } else { # all are missing
        oxy_cal_slopes[i] <- NA
        oxy_cal_ints[i]   <- NA
        oxy_cal_rsq[i]    <- NA
      }
      
      # HYDROGEN
      
      # check to see if sum of is.na() on oxygen data = nrow of oxygen data
      if (sum(is.na(std_subset$d2H_meas_mean)) < nrow(std_subset) &
          sum(is.na(std_subset$d2H_ref_mean)) < nrow(std_subset)) {
        
        tmp <- lm(d2H_ref_mean ~ d2H_meas_mean, data = std_subset)
        
        hyd_cal_slopes[i] <- coef(tmp)[[2]]
        hyd_cal_ints[i]   <- coef(tmp)[[1]]
        hyd_cal_rsq[i]    <- summary(tmp)$r.squared
        
        # enforce thresholds. replace regression parameters where they fail.
        if (!is.na(hyd_cal_rsq[i])) {
          if ((hyd_cal_slopes[i] > (1 + slope_tolerance)) |
              (hyd_cal_slopes[i] < (1 - slope_tolerance)) |
              (hyd_cal_rsq[i] < r2_thres)) {
            
            # set as NA
            hyd_cal_slopes[i] <- NA
            hyd_cal_ints[i]   <- NA
            hyd_cal_rsq[i]    <- NA
          }
        } else {
          # set as NA
          hyd_cal_slopes[i] <- NA
          hyd_cal_ints[i]   <- NA
          hyd_cal_rsq[i]    <- NA
        }
        
      } else { # all are missing
        
        hyd_cal_slopes[i] <- NA
        hyd_cal_ints[i]   <- NA
        hyd_cal_rsq[i]    <- NA
      }
    }

    
    # start time is numeric here at some point - hence, format not necessary 
    # to be specified below.
    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start = lubridate::as_datetime(start_time, tz= 'UTC'),
                      end = lubridate::as_datetime(end_time, tz= 'UTC'),
                      o_slope = as.numeric(oxy_cal_slopes),
                      o_intercept = as.numeric(oxy_cal_ints),
                      o_r2 = as.numeric(oxy_cal_rsq),
                      h_slope = as.numeric(hyd_cal_slopes),
                      h_intercept = as.numeric(hyd_cal_ints),
                      h_r2 = as.numeric(hyd_cal_rsq))
    
  } else { # this branch shouldn't run any more, as it indicates no ref data in *entire* timeseries
    out <- data.frame(start = lubridate::as_datetime(start_date, tz = 'UTC'),
                      end = lubridate::as_datetime(end_date, tz = 'UTC'),
                      o_slope = as.numeric(NA),
                      o_intercept = as.numeric(NA),
                      o_r2 = as.numeric(NA),
                      h_slope = as.numeric(NA),
                      h_intercept = as.numeric(NA),
                      h_r2 = as.numeric(NA))
  }
  
  # check to ensure there are 6 columns.
  # add slope, intercept, r2 columns if missing.
  if (!("o_slope" %in% names(out))) {
    out$o_slope <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("o_intercept" %in% names(out))) {
    out$o_intercept <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("o_r2" %in% names(out))) {
    out$o_r2 <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("h_slope" %in% names(out))) {
    out$h_slope <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("h_intercept" %in% names(out))) {
    out$h_intercept <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("h_r2" %in% names(out))) {
    out$h_r2 <- as.numeric(rep(NA, length(out$start)))
  }
  
  # ensure there are 8 columns in out!
  if (ncol(out) != 8) {
    stop("Wrong number of columns in out.")
  }
  
  return(out)
}
 
