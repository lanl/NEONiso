#' calibrate_water_linreg_bysite
#'
#' This function uses NEON validation data to apply drift corrections to
#' measured ambient water isotope ratios. In brief, ambient water isotope
#' ratios are calibrated by generating regressions using reference water
#' measurements bracketing an ambient period. Three reference waters are
#' measured once per day, with several injections per reference water.
#' Due to memory effects, only the last three are used currently to generate
#' calibration equations. Regressions between measured d18O and d2H values
#' and NEON-provisioned known reference values are generated, and used to
#' calibrate the period of ambient measurements between them if the r2 of
#' the regression is greater than a threshold value (by default, this is 0.95).
#' Most of this function deals with selecting the appropriate calibration data
#' and determining calibration quality. This function also contains a wrapper
#' for \code{calibrate_ambient_water_linreg}, which calibrates the ambient
#' water data using the calibration parameters generated in this function.
#' This function also copies over data in the qfqm and ucrt hdf5 data groups.
#'
#' *IMPORTANT NOTE* Currently this function does not apply a correction for
#' humidity dependence of Picarro isotopic measurements. This is because the
#' data to implement these corrections is not yet publicly available.
#' Caution is suggested when analyzing data at low humidities, below ~5000 ppm,
#' with likely higher biases at lower humidity values. This is expected to be
#' added in Q3 of 2020.
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param site Four-letter NEON code for site being processed.
#' @param inpath Directory path to input (monthly) NEON HDF5 files.
#' @param outpath Directory path to save output data file. (For now, 1 per site).
#' @param force_cal_to_beginning Extend first calibration to
#'                               the beginning of the file?
#' @param force_cal_to_end Extend last calibration to the end of the file?
#' @param r2_thres Minimum r2 threshold of an "acceptable" calibration. Acts to
#'            remove calibration periods where a measurement error makes
#'            relationship nonlinear. Default = 0.95
#' @param filter_data Apply median absolute deviation filter from Brock 86 to
#'             remove impulse spikes?
#' @param calibration_half_width Determines the range of standard measurements
#'             to use in determining the calibration regression dataset. Creates
#'             a moving window that is \code{2*calibration_half_width} days wide.
#'             Default is set to 14 for a 28 day moving window.
#' @param slope_tolerance How different from 1 should we allow 'passing' regression
#'             slopes to be? Experimental parameter, off by default
#'             (e.g., default slope parameter = 9999)
#'
#' @return nothing to the workspace, but creates a new output file of
#'         calibrated water isotope data.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%
#' @importFrom utils tail
#' @import dplyr
#' @import neonUtilities
#' @importFrom data.table rleidv
calibrate_water_linreg_bysite <- function(inpath,
                                   outpath,
                                   site,
                                   calibration_half_width = 14, # days
                                   filter_data = TRUE,
                                   force_cal_to_beginning = TRUE,
                                   force_cal_to_end = TRUE,
                                   r2_thres = 0.95,
                                   slope_tolerance = 9999) {

  # print status.
  print("Processing water calibration data...")
  
  # stack data available for a given site into a single timeseries.
  wiso_ref <- neonUtilities::stackEddy(inpath, level = "dp01", avg = 3)
  
  # extract standards data.
  high <- subset(wiso_ref[[site]], wiso_ref[[site]]$verticalPosition == 'h2oHigh')
  med  <- subset(wiso_ref[[site]], wiso_ref[[site]]$verticalPosition == 'h2oMed')
  low  <- subset(wiso_ref[[site]], wiso_ref[[site]]$verticalPosition == 'h2oLow')

  # attempt to pull relevent data out to a single dataframe.
  high_rs <- data.frame(d18O_meas_mean  = high$data.isoH2o.dlta18OH2o.mean,
                        d18O_meas_var   = high$data.isoH2o.dlta18OH2o.vari,
                        d18O_meas_n     = high$data.isoH2o.dlta18OH2o.numSamp,
                        d18O_ref_mean   = high$data.isoH2o.dlta18OH2oRefe.mean,
                        d18O_ref_var    = high$data.isoH2o.dlta18OH2oRefe.vari,
                        d18O_ref_n      = high$data.isoH2o.dlta18OH2oRefe.numSamp,
                        d2H_meas_mean   = high$data.isoH2o.dlta2HH2o.mean,
                        d2H_meas_var    = high$data.isoH2o.dlta2HH2o.vari,
                        d2H_meas_n      = high$data.isoH2o.dlta2HH2o.numSamp,
                        d2H_ref_mean    = high$data.isoH2o.dlta2HH2oRefe.mean,
                        d2H_ref_var     = high$data.isoH2o.dlta2HH2oRefe.vari,
                        d2H_ref_n       = high$data.isoH2o.dlta2HH2oRefe.numSamp,
                        btime           = high$timeBgn,
                        etime           = high$timeEnd)

  high_rs <- high_rs %>%
    mutate(std_name = "high")
   
  med_rs <- data.frame(d18O_meas_mean   = med$data.isoH2o.dlta18OH2o.mean,
                       d18O_meas_var    = med$data.isoH2o.dlta18OH2o.vari,
                       d18O_meas_n      = med$data.isoH2o.dlta18OH2o.numSamp,
                       d18O_ref_mean    = med$data.isoH2o.dlta18OH2oRefe.mean,
                       d18O_ref_var     = med$data.isoH2o.dlta18OH2oRefe.vari,
                       d18O_ref_n       = med$data.isoH2o.dlta18OH2oRefe.numSamp,
                       d2H_meas_mean    = med$data.isoH2o.dlta2HH2o.mean,
                       d2H_meas_var     = med$data.isoH2o.dlta2HH2o.vari,
                       d2H_meas_n       = med$data.isoH2o.dlta2HH2o.numSamp,
                       d2H_ref_mean     = med$data.isoH2o.dlta2HH2oRefe.mean,
                       d2H_ref_var      = med$data.isoH2o.dlta2HH2oRefe.vari,
                       d2H_ref_n        = med$data.isoH2o.dlta2HH2oRefe.numSamp,
                       btime            = med$timeBgn,
                       etime            = med$timeEnd)

  med_rs <- med_rs %>%
    mutate(std_name = "med")

  low_rs <- data.frame(d18O_meas_mean  = low$data.isoH2o.dlta18OH2o.mean,
                       d18O_meas_var   = low$data.isoH2o.dlta18OH2o.vari,
                       d18O_meas_n     = low$data.isoH2o.dlta18OH2o.numSamp,
                       d18O_ref_mean   = low$data.isoH2o.dlta18OH2oRefe.mean,
                       d18O_ref_var    = low$data.isoH2o.dlta18OH2oRefe.vari,
                       d18O_ref_n      = low$data.isoH2o.dlta18OH2oRefe.numSamp,
                       d2H_meas_mean   = low$data.isoH2o.dlta2HH2o.mean,
                       d2H_meas_var    = low$data.isoH2o.dlta2HH2o.vari,
                       d2H_meas_n      = low$data.isoH2o.dlta2HH2o.numSamp,
                       d2H_ref_mean    = low$data.isoH2o.dlta2HH2oRefe.mean,
                       d2H_ref_var     = low$data.isoH2o.dlta2HH2oRefe.vari,
                       d2H_ref_n       = low$data.isoH2o.dlta2HH2oRefe.numSamp,
                       btime           = low$timeBgn,
                       etime           = low$timeEnd)

  low_rs <- low_rs %>%
    mutate(std_name = "low")

  # add fix for NEON standard swap.
  low_rs <- swap_standard_isotoperatios(low_rs)
  med_rs <- swap_standard_isotoperatios(med_rs)
  high_rs <- swap_standard_isotoperatios(high_rs)
  
  # convert times in these data.frames (btime and etime) to posixct
  low_rs$btime <- as.POSIXct(low_rs$btime,
                           format = "%Y-%m-%dT%H:%M:%OSZ",
                           tz = "UTC")
  low_rs$etime <- as.POSIXct(low_rs$etime,
                           format = "%Y-%m-%dT%H:%M:%OSZ",
                           tz = "UTC")
  med_rs$btime <- as.POSIXct(med_rs$btime,
                           format = "%Y-%m-%dT%H:%M:%OSZ",
                           tz = "UTC")
  med_rs$etime <- as.POSIXct(med_rs$etime,
                           format = "%Y-%m-%dT%H:%M:%OSZ",
                           tz = "UTC")
  high_rs$btime <- as.POSIXct(high_rs$btime,
                           format = "%Y-%m-%dT%H:%M:%OSZ",
                           tz = "UTC")
  high_rs$etime <- as.POSIXct(high_rs$etime,
                           format = "%Y-%m-%dT%H:%M:%OSZ",
                           tz = "UTC")
  
  #--------------------------------------------------------------
  # Ensure same number of measurements for each standard
  #--------------------------------------------------------------
  # add group ids using run length encoding based on time differences.
  thres_hours <- as.difftime("04:00:00", # assume any time difference 
                    format = "%H:%M:%S", # > 4 hours is a new reference measurement
                    units = "mins")
  
  high_rs <- high_rs %>%
    mutate(time_diff = ifelse(btime - lag(btime) > thres_hours, 1, 0))
  high_rs$periods <- rleidv(high_rs, "time_diff") %/% 2
  
  med_rs <- med_rs %>%
    mutate(time_diff = ifelse(btime - lag(btime) > thres_hours, 1, 0))
  med_rs$periods <- rleidv(med_rs, "time_diff") %/% 2
  
  low_rs <- low_rs %>%
    mutate(time_diff = ifelse(btime - lag(btime) > thres_hours, 1, 0))
  low_rs$periods <- rleidv(low_rs, "time_diff") %/% 2
  
  #return(list(low_rs,med_rs,high_rs))

  high_rs <- high_rs %>%
    group_by(periods) %>%
    filter(d18O_meas_n > 30 | is.na(d18O_meas_n)) %>%
    slice_tail(n = 3) %>%
    ungroup()

  med_rs <- med_rs %>%
    group_by(periods) %>%
    filter(d18O_meas_n > 30 | is.na(d18O_meas_n)) %>%
    slice_tail(n = 3) %>%
    ungroup()

  low_rs <- low_rs %>%
    group_by(periods) %>%
    filter(d18O_meas_n > 30 | is.na(d18O_meas_n)) %>%
    slice_tail(n = 3) %>%
    ungroup()

  #=======================================================================
  # apply calibration routines
  #=======================================================================
  # bind together, and cleanup.
  #### OMIT FOR ERROR PROPOAGATION.
  stds <- do.call(rbind, list(high_rs, med_rs, low_rs))
  
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
    loop_start_date <- start_date + lubridate::days(x = calibration_half_width)
    loop_end_date   <- end_date - lubridate::days(x = calibration_half_width)
    
    # generate date sequence
    date_seq <- base::seq.Date(loop_start_date, loop_end_date, by = "1 day")
    
    for (i in 1:length(date_seq)) {
      
      start_time[i] <- as.POSIXct(paste(date_seq[i],"00:00:00"),
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "UTC", origin = "1970-01-01")
      end_time[i]   <- as.POSIXct(paste(date_seq[i],"23:59:59"),
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "UTC", origin = "1970-01-01")
      
      # define calibration interval
      cal_period <- lubridate::interval(date_seq[i] - lubridate::days(calibration_half_width),
                                        date_seq[i] + lubridate::days(calibration_half_width))
      
      # select data subset using the interval
      std_subset <- stds %>%
        dplyr::filter(btime %within% cal_period)
      
      # check to see if sum of is.na() on oxygen data = nrow of oxygen data
      if (sum(is.na(std_subset$d18O_meas_mean)) < nrow(std_subset) &
          sum(is.na(std_subset$d18O_ref_mean)) < nrow(std_subset)) {
        tmp <- lm(d18O_ref_mean ~ d18O_meas_mean, data = std_subset)
        
        oxy_cal_slopes[i] <- coef(tmp)[[2]]
        oxy_cal_ints[i]   <- coef(tmp)[[1]]
        oxy_cal_rsq[i]    <- summary(tmp)$r.squared
        
        # enforce thresholds. replace regression parameters as NA where they fail.
        if ((oxy_cal_slopes[i] > (1 + slope_tolerance)) |
            (oxy_cal_slopes[i] < (1 + slope_tolerance)) |
            (oxy_cal_rsq[i] < r2_thres)) {
          
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
        if ((hyd_cal_slopes[i] > (1 + slope_tolerance)) |
            (hyd_cal_slopes[i] < (1 + slope_tolerance)) |
            (hyd_cal_rsq[i] < r2_thres)) {
          
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

    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start = as.POSIXct(start_time, tz = "UTC", origin = "1970-01-01"),
                      end = as.POSIXct(end_time, tz = "UTC", origin = "1970-01-01"),
                      o_slope = as.numeric(oxy_cal_slopes),
                      o_intercept = as.numeric(oxy_cal_ints),
                      o_r2 = as.numeric(oxy_cal_rsq),
                      h_slope = as.numeric(hyd_cal_slopes),
                      h_intercept = as.numeric(hyd_cal_ints),
                      h_r2 = as.numeric(hyd_cal_rsq))

  } else { # this branch shouldn't run any more, as it indicates no ref data in *entire* timeseries
    out <- data.frame(start = start_date,
                      end = end_date,
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

  var_for_h5 <- out

  var_for_h5$start <- convert_POSIXct_to_NEONhdf5_time(var_for_h5$start)
  var_for_h5$end <- convert_POSIXct_to_NEONhdf5_time(var_for_h5$end)

  var_for_h5$valid_period_start <- var_for_h5$start
  var_for_h5$valid_period_end   <- var_for_h5$end
  
  # remove old vars.
  var_for_h5$start <- var_for_h5$end <- NULL

  #----------------------------------
  # write out to h5 file.
  #----------------------------------
  # generate file name:
  outname <- paste0(outpath,"/NEON.",site,".wiso.alldata.calibrated.",
                    2*calibration_half_width,"dayWindow.h5")

  rhdf5::h5createFile(outname)
  rhdf5::h5createGroup(outname, paste0("/", site))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/isoH2o"))

  # okay try to write out to h5 file.
  fid <- rhdf5::H5Fopen(outname)

  #####------------NEED TO PORT OVER TO NEW FUNCTION------------------
  # # copy attributes from source file and write to output file.
  # tmp <- rhdf5::h5readAttributes(inname, paste0("/", site))
  # attrloc <- rhdf5::H5Gopen(fid, paste0("/", site))
  # 
  # for (i in 1:length(tmp)) { # probably a more rapid way to do this...lapply?
  #   rhdf5::h5writeAttribute(h5obj = attrloc,
  #                           attr = tmp[[i]],
  #                           name = names(tmp)[i])
  # }
  # 
  # add attributes regarding sloep and r2 thresholds
  
  #rhdf5::H5Gclose(attrloc)

  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/isoH2o/calData"))

  h2o_cal_outloc <- rhdf5::H5Gopen(fid,
                              paste0("/", site, "/dp01/data/isoH2o/calData"))

  # write out dataset.
  rhdf5::h5writeDataset.data.frame(obj = var_for_h5,
                                   h5loc = h2o_cal_outloc,
                                   name = "calRegressions",
                                   DataFrameAsCompound = TRUE)

  # close the group and the file
  rhdf5::H5Gclose(h2o_cal_outloc)
  # 
  #####------------NEED TO PORT OVER TO NEW FUNCTION------------------
  
  # stack data available for a given site into a single timeseries.
  # should probably kick this out to its own function someday.

  # #---------------------------------------------
  # #---------------------------------------------
  # # copy high/mid/low standard data from input file.
  # #---------------------------------------------
  # #---------------------------------------------
  # #low
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))

  low_outloc <- rhdf5::H5Gopen(fid,
                           paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))
  

  dlta18OH2o <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta18OH2o.")) %>%
    rename(mean = data.isoH2o.dlta18OH2o.mean,
           min  = data.isoH2o.dlta18OH2o.min,
           max  = data.isoH2o.dlta18OH2o.max,
           vari = data.isoH2o.dlta18OH2o.vari,
           numSamp = data.isoH2o.dlta18OH2o.numSamp) %>%
    mutate(varname = "dlta18OH2o")

  dlta2HH2o <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta2HH2o.")) %>%
    rename(mean = data.isoH2o.dlta2HH2o.mean,
           min  = data.isoH2o.dlta2HH2o.min,
           max  = data.isoH2o.dlta2HH2o.max,
           vari = data.isoH2o.dlta2HH2o.vari,
           numSamp = data.isoH2o.dlta2HH2o.numSamp) %>%
    mutate(varname = "dlta2HH2o")
  
  dlta18OH2oRefe <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta18OH2oRefe.")) %>%
    rename(mean = data.isoH2o.dlta18OH2oRefe.mean,
           min  = data.isoH2o.dlta18OH2oRefe.min,
           max  = data.isoH2o.dlta18OH2oRefe.max,
           vari = data.isoH2o.dlta18OH2oRefe.vari,
           numSamp = data.isoH2o.dlta18OH2oRefe.numSamp) %>%
    mutate(varname = "dlta18OH2oRefe")
  
  dlta2HH2oRefe <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta2HH2oRefe.")) %>%
    rename(mean = data.isoH2o.dlta2HH2oRefe.mean,
           min  = data.isoH2o.dlta2HH2oRefe.min,
           max  = data.isoH2o.dlta2HH2oRefe.max,
           vari = data.isoH2o.dlta2HH2oRefe.vari,
           numSamp = data.isoH2o.dlta2HH2oRefe.numSamp) %>%
    mutate(varname = "dlta2HH2oRefe")

  pres <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.pres.")) %>%
    rename(mean = data.isoH2o.pres.mean,
           min  = data.isoH2o.pres.min,
           max  = data.isoH2o.pres.max,
           vari = data.isoH2o.pres.vari,
           numSamp = data.isoH2o.pres.numSamp) %>%
    mutate(varname = "pres")
  
  presEnvHut <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.presEnvHut.")) %>%
    rename(mean = data.isoH2o.presEnvHut.mean,
           min  = data.isoH2o.presEnvHut.min,
           max  = data.isoH2o.presEnvHut.max,
           vari = data.isoH2o.presEnvHut.vari,
           numSamp = data.isoH2o.presEnvHut.numSamp) %>%
    mutate(varname = "presEnvHut")
  
  rhEnvHut <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.rhEnvHut.")) %>%
    rename(mean = data.isoH2o.rhEnvHut.mean,
           min  = data.isoH2o.rhEnvHut.min,
           max  = data.isoH2o.rhEnvHut.max,
           vari = data.isoH2o.rhEnvHut.vari,
           numSamp = data.isoH2o.rhEnvHut.numSamp) %>%
    mutate(varname = "rhEnvHut")
  
  rtioMoleWetH2o <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.rtioMoleWetH2o.")) %>%
    rename(mean = data.isoH2o.rtioMoleWetH2o.mean,
           min  = data.isoH2o.rtioMoleWetH2o.min,
           max  = data.isoH2o.rtioMoleWetH2o.max,
           vari = data.isoH2o.rtioMoleWetH2o.vari,
           numSamp = data.isoH2o.rtioMoleWetH2o.numSamp) %>%
    mutate(varname = "rtioMoleWetH2o")
  
  rtioMoleWetH2oEnvHut <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.rtioMoleWetH2oEnvHut.")) %>%
    rename(mean = data.isoH2o.rtioMoleWetH2oEnvHut.mean,
           min  = data.isoH2o.rtioMoleWetH2oEnvHut.min,
           max  = data.isoH2o.rtioMoleWetH2oEnvHut.max,
           vari = data.isoH2o.rtioMoleWetH2oEnvHut.vari,
           numSamp = data.isoH2o.rtioMoleWetH2oEnvHut.numSamp) %>%
    mutate(varname = "rtioMoleWetH2oEnvHut")
  
  temp <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.temp.")) %>%
    rename(mean = data.isoH2o.temp.mean,
           min  = data.isoH2o.temp.min,
           max  = data.isoH2o.temp.max,
           vari = data.isoH2o.temp.vari,
           numSamp = data.isoH2o.temp.numSamp) %>%
    mutate(varname = "temp")
  
  tempEnvHut <- low %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.tempEnvHut.")) %>%
    rename(mean = data.isoH2o.tempEnvHut.mean,
           min  = data.isoH2o.tempEnvHut.min,
           max  = data.isoH2o.tempEnvHut.max,
           vari = data.isoH2o.tempEnvHut.vari,
           numSamp = data.isoH2o.tempEnvHut.numSamp) %>%
    mutate(varname = "tempEnvHut")
  
  data_out_all <- do.call(rbind,list(dlta18OH2o, dlta2HH2o, dlta18OH2oRefe, dlta2HH2oRefe,
                                     pres, presEnvHut, rhEnvHut,
                                     rtioMoleWetH2o, rtioMoleWetH2oEnvHut, temp, tempEnvHut))
  
  lowref <- base::split(data_out_all, factor(data_out_all$varname))
  
  lowref <- calibrate_standards_water(out, lowref)

  # and write out as a dataframe.
  lapply(names(lowref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = lowref[[x]],
                                     h5loc = low_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})

  rhdf5::H5Gclose(low_outloc)
  # 
  # #------------------------------------------------------------
  # #medium
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))
  
  med_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))
  
  dlta18OH2o <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta18OH2o.")) %>%
    rename(mean = data.isoH2o.dlta18OH2o.mean,
           min  = data.isoH2o.dlta18OH2o.min,
           max  = data.isoH2o.dlta18OH2o.max,
           vari = data.isoH2o.dlta18OH2o.vari,
           numSamp = data.isoH2o.dlta18OH2o.numSamp) %>%
    mutate(varname = "dlta18OH2o")
  
  dlta2HH2o <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta2HH2o.")) %>%
    rename(mean = data.isoH2o.dlta2HH2o.mean,
           min  = data.isoH2o.dlta2HH2o.min,
           max  = data.isoH2o.dlta2HH2o.max,
           vari = data.isoH2o.dlta2HH2o.vari,
           numSamp = data.isoH2o.dlta2HH2o.numSamp) %>%
    mutate(varname = "dlta2HH2o")
  
  dlta18OH2oRefe <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta18OH2oRefe.")) %>%
    rename(mean = data.isoH2o.dlta18OH2oRefe.mean,
           min  = data.isoH2o.dlta18OH2oRefe.min,
           max  = data.isoH2o.dlta18OH2oRefe.max,
           vari = data.isoH2o.dlta18OH2oRefe.vari,
           numSamp = data.isoH2o.dlta18OH2oRefe.numSamp) %>%
    mutate(varname = "dlta18OH2oRefe")
  
  dlta2HH2oRefe <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta2HH2oRefe.")) %>%
    rename(mean = data.isoH2o.dlta2HH2oRefe.mean,
           min  = data.isoH2o.dlta2HH2oRefe.min,
           max  = data.isoH2o.dlta2HH2oRefe.max,
           vari = data.isoH2o.dlta2HH2oRefe.vari,
           numSamp = data.isoH2o.dlta2HH2oRefe.numSamp) %>%
    mutate(varname = "dlta2HH2oRefe")
  
  pres <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.pres.")) %>%
    rename(mean = data.isoH2o.pres.mean,
           min  = data.isoH2o.pres.min,
           max  = data.isoH2o.pres.max,
           vari = data.isoH2o.pres.vari,
           numSamp = data.isoH2o.pres.numSamp) %>%
    mutate(varname = "pres")
  
  presEnvHut <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.presEnvHut.")) %>%
    rename(mean = data.isoH2o.presEnvHut.mean,
           min  = data.isoH2o.presEnvHut.min,
           max  = data.isoH2o.presEnvHut.max,
           vari = data.isoH2o.presEnvHut.vari,
           numSamp = data.isoH2o.presEnvHut.numSamp) %>%
    mutate(varname = "presEnvHut")
  
  rhEnvHut <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.rhEnvHut.")) %>%
    rename(mean = data.isoH2o.rhEnvHut.mean,
           min  = data.isoH2o.rhEnvHut.min,
           max  = data.isoH2o.rhEnvHut.max,
           vari = data.isoH2o.rhEnvHut.vari,
           numSamp = data.isoH2o.rhEnvHut.numSamp) %>%
    mutate(varname = "rhEnvHut")
  
  rtioMoleWetH2o <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.rtioMoleWetH2o.")) %>%
    rename(mean = data.isoH2o.rtioMoleWetH2o.mean,
           min  = data.isoH2o.rtioMoleWetH2o.min,
           max  = data.isoH2o.rtioMoleWetH2o.max,
           vari = data.isoH2o.rtioMoleWetH2o.vari,
           numSamp = data.isoH2o.rtioMoleWetH2o.numSamp) %>%
    mutate(varname = "rtioMoleWetH2o")
  
  rtioMoleWetH2oEnvHut <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.rtioMoleWetH2oEnvHut.")) %>%
    rename(mean = data.isoH2o.rtioMoleWetH2oEnvHut.mean,
           min  = data.isoH2o.rtioMoleWetH2oEnvHut.min,
           max  = data.isoH2o.rtioMoleWetH2oEnvHut.max,
           vari = data.isoH2o.rtioMoleWetH2oEnvHut.vari,
           numSamp = data.isoH2o.rtioMoleWetH2oEnvHut.numSamp) %>%
    mutate(varname = "rtioMoleWetH2oEnvHut")
  
  temp <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.temp.")) %>%
    rename(mean = data.isoH2o.temp.mean,
           min  = data.isoH2o.temp.min,
           max  = data.isoH2o.temp.max,
           vari = data.isoH2o.temp.vari,
           numSamp = data.isoH2o.temp.numSamp) %>%
    mutate(varname = "temp")
  
  tempEnvHut <- med %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.tempEnvHut.")) %>%
    rename(mean = data.isoH2o.tempEnvHut.mean,
           min  = data.isoH2o.tempEnvHut.min,
           max  = data.isoH2o.tempEnvHut.max,
           vari = data.isoH2o.tempEnvHut.vari,
           numSamp = data.isoH2o.tempEnvHut.numSamp) %>%
    mutate(varname = "tempEnvHut")
  
  data_out_all <- do.call(rbind,list(dlta18OH2o, dlta2HH2o, dlta18OH2oRefe, dlta2HH2oRefe,
                                     pres, presEnvHut, rhEnvHut,
                                     rtioMoleWetH2o, rtioMoleWetH2oEnvHut, temp, tempEnvHut))
  
  medref <- base::split(data_out_all, factor(data_out_all$varname))
  
  medref <- calibrate_standards_water(out, medref)
  
  # loop through each of the variables in list amb.data.list
  # and write out as a dataframe.
  lapply(names(medref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = medref[[x]],
                                     h5loc = med_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(med_outloc)
  
  # #------------------------------------------------------------
  # #high
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oHigh_03m"))

  high_outloc <- rhdf5::H5Gopen(fid,
                       paste0("/", site, "/dp01/data/isoH2o/h2oHigh_03m"))

  
  dlta18OH2o <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta18OH2o.")) %>%
    rename(mean = data.isoH2o.dlta18OH2o.mean,
           min  = data.isoH2o.dlta18OH2o.min,
           max  = data.isoH2o.dlta18OH2o.max,
           vari = data.isoH2o.dlta18OH2o.vari,
           numSamp = data.isoH2o.dlta18OH2o.numSamp) %>%
    mutate(varname = "dlta18OH2o")
  
  dlta2HH2o <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta2HH2o.")) %>%
    rename(mean = data.isoH2o.dlta2HH2o.mean,
           min  = data.isoH2o.dlta2HH2o.min,
           max  = data.isoH2o.dlta2HH2o.max,
           vari = data.isoH2o.dlta2HH2o.vari,
           numSamp = data.isoH2o.dlta2HH2o.numSamp) %>%
    mutate(varname = "dlta2HH2o")
  
  dlta18OH2oRefe <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta18OH2oRefe.")) %>%
    rename(mean = data.isoH2o.dlta18OH2oRefe.mean,
           min  = data.isoH2o.dlta18OH2oRefe.min,
           max  = data.isoH2o.dlta18OH2oRefe.max,
           vari = data.isoH2o.dlta18OH2oRefe.vari,
           numSamp = data.isoH2o.dlta18OH2oRefe.numSamp) %>%
    mutate(varname = "dlta18OH2oRefe")
  
  dlta2HH2oRefe <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.dlta2HH2oRefe.")) %>%
    rename(mean = data.isoH2o.dlta2HH2oRefe.mean,
           min  = data.isoH2o.dlta2HH2oRefe.min,
           max  = data.isoH2o.dlta2HH2oRefe.max,
           vari = data.isoH2o.dlta2HH2oRefe.vari,
           numSamp = data.isoH2o.dlta2HH2oRefe.numSamp) %>%
    mutate(varname = "dlta2HH2oRefe")
  
  pres <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.pres.")) %>%
    rename(mean = data.isoH2o.pres.mean,
           min  = data.isoH2o.pres.min,
           max  = data.isoH2o.pres.max,
           vari = data.isoH2o.pres.vari,
           numSamp = data.isoH2o.pres.numSamp) %>%
    mutate(varname = "pres")
  
  presEnvHut <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.presEnvHut.")) %>%
    rename(mean = data.isoH2o.presEnvHut.mean,
           min  = data.isoH2o.presEnvHut.min,
           max  = data.isoH2o.presEnvHut.max,
           vari = data.isoH2o.presEnvHut.vari,
           numSamp = data.isoH2o.presEnvHut.numSamp) %>%
    mutate(varname = "presEnvHut")
  
  rhEnvHut <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.rhEnvHut.")) %>%
    rename(mean = data.isoH2o.rhEnvHut.mean,
           min  = data.isoH2o.rhEnvHut.min,
           max  = data.isoH2o.rhEnvHut.max,
           vari = data.isoH2o.rhEnvHut.vari,
           numSamp = data.isoH2o.rhEnvHut.numSamp) %>%
    mutate(varname = "rhEnvHut")
  
  rtioMoleWetH2o <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.rtioMoleWetH2o.")) %>%
    rename(mean = data.isoH2o.rtioMoleWetH2o.mean,
           min  = data.isoH2o.rtioMoleWetH2o.min,
           max  = data.isoH2o.rtioMoleWetH2o.max,
           vari = data.isoH2o.rtioMoleWetH2o.vari,
           numSamp = data.isoH2o.rtioMoleWetH2o.numSamp) %>%
    mutate(varname = "rtioMoleWetH2o")
  
  rtioMoleWetH2oEnvHut <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.rtioMoleWetH2oEnvHut.")) %>%
    rename(mean = data.isoH2o.rtioMoleWetH2oEnvHut.mean,
           min  = data.isoH2o.rtioMoleWetH2oEnvHut.min,
           max  = data.isoH2o.rtioMoleWetH2oEnvHut.max,
           vari = data.isoH2o.rtioMoleWetH2oEnvHut.vari,
           numSamp = data.isoH2o.rtioMoleWetH2oEnvHut.numSamp) %>%
    mutate(varname = "rtioMoleWetH2oEnvHut")
  
  temp <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.temp.")) %>%
    rename(mean = data.isoH2o.temp.mean,
           min  = data.isoH2o.temp.min,
           max  = data.isoH2o.temp.max,
           vari = data.isoH2o.temp.vari,
           numSamp = data.isoH2o.temp.numSamp) %>%
    mutate(varname = "temp")
  
  tempEnvHut <- high %>%
    select(timeBgn,timeEnd,starts_with("data.isoH2o.tempEnvHut.")) %>%
    rename(mean = data.isoH2o.tempEnvHut.mean,
           min  = data.isoH2o.tempEnvHut.min,
           max  = data.isoH2o.tempEnvHut.max,
           vari = data.isoH2o.tempEnvHut.vari,
           numSamp = data.isoH2o.tempEnvHut.numSamp) %>%
    mutate(varname = "tempEnvHut")
  
  data_out_all <- do.call(rbind,list(dlta18OH2o, dlta2HH2o, dlta18OH2oRefe, dlta2HH2oRefe,
                                     pres, presEnvHut, rhEnvHut,
                                     rtioMoleWetH2o, rtioMoleWetH2oEnvHut, temp, tempEnvHut))
  
  highref <- base::split(data_out_all, factor(data_out_all$varname))
  
  highref <- calibrate_standards_water(out, highref)

  # loop through each of the variables in list amb.data.list
  # and write out as a dataframe.
  lapply(names(highref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = highref[[x]],
                                     h5loc = high_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})

  rhdf5::H5Gclose(high_outloc)

  # close the group and the file
  rhdf5::H5Fclose(fid)
  Sys.sleep(0.5)

  rhdf5::h5closeAll()
  # 
  # fid <- rhdf5::H5Fopen(outname)
  # 
  # # copy attributes from source file and write to output file.
  # tmp <- rhdf5::h5readAttributes(inname, paste0("/", site))
  # attrloc <- rhdf5::H5Gopen(fid, paste0("/", site))
  # 
  # for (i in 1:length(tmp)) { # probably a more rapid way to do this...lapply?
  #   rhdf5::h5writeAttribute(h5obj = attrloc,
  #                           attr = tmp[[i]],
  #                           name = names(tmp)[i])
  # }
  # 
  # rhdf5::H5Gclose(attrloc)
  # 
  #===========================================================
  # calibrate data for each height.
  #-------------------------------------
  # stack data to get ambient observations.
  print("stacking ambient data...this may take a while...")

  # stack data available for a given site into a single timeseries.
  # should probably kick this out to its own function someday.

  dlta18O_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "dlta18OH2o", avg = 9)
  dlta18OH2o <- dlta18O_list[[site]] %>%
    select(verticalPosition,timeBgn,timeEnd,contains("data.isoH2o")) %>%
    filter(!(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch"))) %>%
    rename(mean = data.isoH2o.dlta18OH2o.mean,
           min  = data.isoH2o.dlta18OH2o.min,
           max  = data.isoH2o.dlta18OH2o.max,
           vari = data.isoH2o.dlta18OH2o.vari,
           numSamp = data.isoH2o.dlta18OH2o.numSamp) %>%
    mutate(varname = "dlta18OH2o")

  dlta2H_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "dlta2HH2o", avg = 9)
  dlta2HH2o <- dlta2H_list[[site]] %>%
    select(verticalPosition,timeBgn,timeEnd,contains("data.isoH2o")) %>%
    filter(!(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch"))) %>%
    rename(mean = data.isoH2o.dlta2HH2o.mean,
           min  = data.isoH2o.dlta2HH2o.min,
           max  = data.isoH2o.dlta2HH2o.max,
           vari = data.isoH2o.dlta2HH2o.vari,
           numSamp = data.isoH2o.dlta2HH2o.numSamp) %>%
    mutate(varname = "dlta2HH2o")

  pres_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "pres", avg = 9)
  pres <- pres_list[[site]] %>%
    select(verticalPosition,timeBgn,timeEnd,contains("data.isoH2o")) %>%
    filter(!(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch"))) %>%
    rename(mean = data.isoH2o.pres.mean,
           min  = data.isoH2o.pres.min,
           max  = data.isoH2o.pres.max,
           vari = data.isoH2o.pres.vari,
           numSamp = data.isoH2o.pres.numSamp) %>%
    mutate(varname = "pres")

  presEnvHut_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "presEnvHut", avg = 9)
  presEnvHut <- presEnvHut_list[[site]] %>%
    select(verticalPosition,timeBgn,timeEnd,contains("data.isoH2o")) %>%
    filter(!(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch"))) %>%
    rename(mean = data.isoH2o.presEnvHut.mean,
           min  = data.isoH2o.presEnvHut.min,
           max  = data.isoH2o.presEnvHut.max,
           vari = data.isoH2o.presEnvHut.vari,
           numSamp = data.isoH2o.presEnvHut.numSamp) %>%
    mutate(varname = "presEnvHut")

  rhEnvHut_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "rhEnvHut", avg = 9)
  rhEnvHut <- rhEnvHut_list[[site]] %>%
    select(verticalPosition,timeBgn,timeEnd,contains("data.isoH2o")) %>%
    filter(!(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch"))) %>%
    rename(mean = data.isoH2o.rhEnvHut.mean,
           min  = data.isoH2o.rhEnvHut.min,
           max  = data.isoH2o.rhEnvHut.max,
           vari = data.isoH2o.rhEnvHut.vari,
           numSamp = data.isoH2o.rhEnvHut.numSamp) %>%
    mutate(varname = "rhEnvHut")

  rtioMoleWetH2o_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "rtioMoleWetH2o", avg = 9)
  rtioMoleWetH2o <- rtioMoleWetH2o_list[[site]] %>%
    select(verticalPosition,timeBgn,timeEnd,contains("data.isoH2o")) %>%
    filter(!(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch"))) %>%
    rename(mean = data.isoH2o.rtioMoleWetH2o.mean,
           min  = data.isoH2o.rtioMoleWetH2o.min,
           max  = data.isoH2o.rtioMoleWetH2o.max,
           vari = data.isoH2o.rtioMoleWetH2o.vari,
           numSamp = data.isoH2o.rtioMoleWetH2o.numSamp) %>%
    mutate(varname = "rtioMoleWetH2o")

  rtioMoleWetH2oEnvHut_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "rtioMoleWetH2oEnvHut", avg = 9)
  rtioMoleWetH2oEnvHut <- rtioMoleWetH2oEnvHut_list[[site]] %>%
    select(verticalPosition,timeBgn,timeEnd,contains("data.isoH2o")) %>%
    filter(!(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch"))) %>%
    rename(mean = data.isoH2o.rtioMoleWetH2oEnvHut.mean,
           min  = data.isoH2o.rtioMoleWetH2oEnvHut.min,
           max  = data.isoH2o.rtioMoleWetH2oEnvHut.max,
           vari = data.isoH2o.rtioMoleWetH2oEnvHut.vari,
           numSamp = data.isoH2o.rtioMoleWetH2oEnvHut.numSamp) %>%
    mutate(varname = "rtioMoleWetH2oEnvHut")

  temp_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "temp", avg = 9)
  temp <- temp_list[[site]] %>%
    select(verticalPosition,timeBgn,timeEnd,contains("data.isoH2o")) %>%
    filter(!(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch"))) %>%
    rename(mean = data.isoH2o.temp.mean,
           min  = data.isoH2o.temp.min,
           max  = data.isoH2o.temp.max,
           vari = data.isoH2o.temp.vari,
           numSamp = data.isoH2o.temp.numSamp) %>%
    mutate(varname = "temp")

  tempEnvHut_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "tempEnvHut", avg = 9)
  tempEnvHut <- tempEnvHut_list[[site]] %>%
    select(verticalPosition,timeBgn,timeEnd,contains("data.isoH2o")) %>%
    filter(!(verticalPosition %in% c("co2Low","co2Med","co2High","co2Arch"))) %>%
    rename(mean = data.isoH2o.tempEnvHut.mean,
           min  = data.isoH2o.tempEnvHut.min,
           max  = data.isoH2o.tempEnvHut.max,
           vari = data.isoH2o.tempEnvHut.vari,
           numSamp = data.isoH2o.tempEnvHut.numSamp) %>%
    mutate(varname = "tempEnvHut")

  data_out_all <- do.call(rbind,list(dlta18OH2o, dlta2HH2o, pres, presEnvHut, rhEnvHut,
                       rtioMoleWetH2o, rtioMoleWetH2oEnvHut, temp, tempEnvHut))

  # split first by height
  data_by_height <- base::split(data_out_all, factor(data_out_all$verticalPosition))

  # get number of heights
  heights <- unique(data_out_all$verticalPosition)
  names_vector <- vector()
  for (i in 1:length(heights)) {
    names_vector[i] <- paste0("000_0",i,"0_09m")
  }

  names(data_by_height) <- names_vector

  # remove verticalPosition column
  data_by_height <- lapply(data_by_height, function(x){dplyr::select(x,-verticalPosition)})

  data_by_height_by_var <- lapply(data_by_height, function(x){base::split(x, factor(x$varname))})

  # okay, now calibrate the ambient data...
  lapply(names(data_by_height_by_var),
         function(x) {
           data_by_height_by_var[[x]] <- lapply(data_by_height_by_var[[x]], function(y){dplyr::select(y,-varname)})
           calibrate_ambient_water_linreg(amb_data_list = data_by_height_by_var[[x]],
                                                    caldf = out,
                                                    outname = x,
                                                    file = outname,
                                                    site = site,
                                                    filter_data = filter_data,
                                                    force_to_end = force_cal_to_end,
                                                    force_to_beginning = force_cal_to_beginning,
                                                    r2_thres = r2_thres)})

  rhdf5::h5closeAll()

  #####------------NEED TO PORT OVER TO NEW FUNCTION------------------
  # print("Copying qfqm...")
  # # copy over ucrt and qfqm groups as well.
  # rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/"))
  # rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/isoH2o"))
  # qfqm <- rhdf5::h5read(inname, paste0("/", site, "/dp01/qfqm/isoH2o"))
  # 
  # lapply(names(qfqm), function(x) {
  #   copy_qfqm_group(data_list = qfqm[[x]],
  #                 outname = x, file = outname, site = site, species = "H2O")})
  # 
  # rhdf5::h5closeAll()
  # 
  # print("Copying ucrt...")
  # # now ucrt.
  # rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/"))
  # rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/isoH2o"))
  # ucrt <- rhdf5::h5read(inname, paste0("/", site, "/dp01/ucrt/isoH2o"))
  # 
  # lapply(names(ucrt), function(x) {
  #   copy_ucrt_group(data_list = ucrt[[x]],
  #                 outname = x, file = outname, site = site, species = "H2O")})
  # 
  # rhdf5::h5closeAll()

}
