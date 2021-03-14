#' fit_carbon_regression
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param method Are we using the Bowling et al. 2003 method
#'              ("Bowling_2003") or direct linear regression of
#'              d13C and CO2 mole fractions ("linreg")?
#' @param calibration_half_width Determines the period (in days)
#'        from which reference data are selected (period
#'        is 2*calibration_half_width).
#' @param ref_data Reference data.frame from which to estimate 
#'        calibration parameters.
#'
#' @return Returns a data.frame of calibration parameters. If
#'        \code{method == "Bowling_2003"}, then data.frame includes 
#'        gain and offset parameters for 12CO2 and 13CO2, and r^2
#'        values for each regression. If \code{method == "linreg"},
#'        then data.frame includes slope, intercept, and r^2 values
#'        for d13C and CO2 values.
#'
#' @importFrom stats coef lm

fit_carbon_regression <- function(ref_data, method, calibration_half_width) {
  
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
                        r2_13C = as.numeric(NA))
    } else {
      
      out <- data.frame(gain12C   = numeric(length = 2e5),
                        gain13C   = numeric(length = 2e5),
                        offset12C = numeric(length = 2e5),
                        offset13C = numeric(length = 2e5),
                        r2_12C    = numeric(length = 2e5),
                        r2_13C    = numeric(length = 2e5))
      
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
          
        } else {
          
          out$gain12C[i]   <- NA
          out$gain13C[i]   <- NA
          out$offset12C[i] <- NA
          out$offset13C[i] <- NA
          out$r2_12C[i]    <- NA
          out$r2_13C[i]    <- NA
        }
      }

      
      #subset out data frame.
      out <- out[1:length(start_time),]
      
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out$timeBgn <- as.POSIXct(start_time, tz = "UTC", origin = "1970-01-01")
      out$timeEnd <- as.POSIXct(end_time, tz = "UTC", origin = "1970-01-01")
      
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
                        co2_slope      = as.numeric(NA),
                        co2_intercept  = as.numeric(NA),
                        co2_r2         = as.numeric(NA))
    } else {
      
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out <- data.frame(d13C_slope     = numeric(length = 2e5),
                        d13C_intercept = numeric(length = 2e5),
                        d13C_r2        = numeric(length = 2e5),
                        co2_slope      = numeric(length = 2e5),
                        co2_intercept  = numeric(length = 2e5),
                        co2_r2         = numeric(length = 2e5))
      
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
          
        } else {
          
          out$d13C_slope[i]     <- NA
          out$d13C_intercept[i] <- NA
          out$d13C_r2[i]        <- NA
          
          out$co2_slope[i]      <- NA
          out$co2_intercept[i]  <- NA
          out$co2_r2[i]         <- NA
          
        }
      }

      #subset out data frame.
      out <- out[1:length(start_time),]
      
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out$timeBgn <- as.POSIXct(start_time, tz = "UTC", origin = "1970-01-01")
      out$timeEnd <- as.POSIXct(end_time, tz = "UTC", origin = "1970-01-01")
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
  
  return(out)
    
}
  