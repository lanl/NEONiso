

fit_carbon_regression <- function(ref_data,...) {
  if (method == "Bowling_2003") {
    if (nrow(refe) == 0) {
      
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out <- data.frame(start = as.POSIXct(as.Date("1970-01-01"), # this won't work w/ stackEddy and needs to be fixed.
                                           tz = "UTC",
                                           origin = "1970-01-01"),
                        end = as.POSIXct(as.Date("1970-01-01"),
                                         tz = "UTC",
                                         origin = "1970-01-01"),
                        gain12C = as.numeric(NA),
                        gain13C = as.numeric(NA),
                        offset12C = as.numeric(NA),
                        offset13C = as.numeric(NA),
                        r2_12C = as.numeric(NA),
                        r2_13C = as.numeric(NA))
    } else {
      
      
      # assign a vector corresponding to calibration period.
      stds$cal_period <- stds$d13C_obs_n
      
      period_id <- 1
      tdiffs <- c(diff(stds$d13C_obs_btime), 0)
      
      # enforce units of tdiffs to be seconds, otherwise it
      # will occasionally be minutes and produce incorrect output.
      units(tdiffs) <- "secs"
      
      for (i in 1:nrow(stds)) {
        
        stds$cal_period[i] <- period_id
        if (tdiffs[i] >= time_diff_btwn_cals) {
          period_id <- period_id + 1
        }
      }
      
      # okay, now run calibrations...
      #------------------------------
      # create output variables.
      gain12C   <- vector()
      gain13C   <- vector()
      offset12C <- vector()
      offset13C <- vector()
      r2_12C    <- vector()
      r2_13C    <- vector()
      
      for (i in 2:max(stds$cal_period)) {
        
        # subset data.
        cal_subset <- stds[which(stds$cal_period == i |
                                   stds$cal_period == (i - 1)), ]
        
        #---------------------------------------------
        # do some light validation of these points.
        cal_subset <- cal_subset %>%
          dplyr::filter(.data$d13C_obs_var < 5 &
                          abs(.data$CO2_obs_mean - .data$CO2_ref_mean) < 10 &
                          abs(.data$d13C_obs_mean - .data$d13C_ref_mean) < 5)
        
        if (length(unique(cal_subset$std_name)) >= 2 & # >= 2 standards
            !all(is.na(cal_subset$d13C_obs_mean)) & # not all obs missing
            !all(is.na(cal_subset$d13C_ref_mean))) { # not all ref missing
          
          tmpmod12C <- stats::lm(conc12CCO2_ref ~ conc12CCO2_obs, data = cal_subset)
          tmpmod13C <- stats::lm(conc13CCO2_ref ~ conc13CCO2_obs, data = cal_subset)
          
          # calculate gain and offset values.
          gain12C[i - 1]   <- stats::coef(tmpmod12C)[[2]]
          gain13C[i - 1]   <- stats::coef(tmpmod13C)[[2]]
          offset12C[i - 1] <- stats::coef(tmpmod12C)[[1]]
          offset13C[i - 1] <- stats::coef(tmpmod13C)[[1]]
          
          # extract r2
          r2_12C[i - 1] <- summary(tmpmod12C)$r.squared
          r2_13C[i - 1] <- summary(tmpmod13C)$r.squared
          
        } else {
          
          gain12C[i - 1]   <- NA
          gain13C[i - 1]   <- NA
          offset12C[i - 1] <- NA
          offset13C[i - 1] <- NA
          r2_12C[i - 1]    <- NA
          r2_13C[i - 1]    <- NA
        }
      }
      
      # make dataframe of calibration data.
      times <- stds %>%
        dplyr::select(.data$d13C_obs_btime, .data$d13C_obs_etime, .data$d13C_ref_btime,
                      .data$d13C_ref_etime, .data$cal_period) %>%
        dplyr::group_by(.data$cal_period) %>%
        dplyr::summarize(etime = max(c(.data$d13C_obs_etime, .data$d13C_ref_etime)))
      
      # loop through times, assign begin/end values
      starttimes <- vector()
      endtimes   <- vector()
      
      for (i in 1:length(gain12C)) {
        starttimes[i] <- times$etime[i]
        endtimes[i]   <- times$etime[i + 1]
      }
      
      # output dataframe giving valid time range, slopes, intercepts, rsquared.
      out <- data.frame(start = as.POSIXct(starttimes,
                                           tz = "UTC",
                                           origin = "1970-01-01"),
                        end = as.POSIXct(endtimes,
                                         tz = "UTC",
                                         origin = "1970-01-01"),
                        gain12C,
                        gain13C,
                        offset12C,
                        offset13C,
                        r2_12C,
                        r2_13C)
      
    }
    
  }
  
}
