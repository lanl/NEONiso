#' calibrate_water
#'
#' `r lifecycle::badge("experimental")`
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
#' for `calibrate_ambient_water_linreg`, which calibrates the ambient
#' water data using the calibration parameters generated in this function.
#' This function also copies over data in the qfqm and ucrt hdf5 data groups.
#'
#' *IMPORTANT NOTE* Currently this function does not apply a correction for
#' humidity dependence of Picarro isotopic measurements. This is because the
#' data to implement these corrections is not yet publicly available.
#' Caution is suggested when analyzing data at low humidities, below ~5000 ppm,
#' with likely higher biases at lower humidity values.
#' 
#' Additionally, please note that this function is meant to work on *all* files
#' for a given site at the same time. A more flexible version that can handle all
#' files or monthly files will be added to a future release.
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
#'             a moving window that is `2*calibration_half_width` days wide.
#'             Default is set to 14 for a 28 day moving window.
#' @param slope_tolerance How different from 1 should we allow 'passing' regression
#'             slopes to be? Experimental parameter, off by default
#'             (e.g., default slope parameter = 9999)
#'
#' @return nothing to the workspace, but creates a new output file of
#'         calibrated water isotope data.
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%
#' @importFrom utils tail
#' @import dplyr
#' @import neonUtilities
#' @importFrom data.table rleidv
#' @importFrom utils packageVersion
calibrate_water       <- function(inpath,
                                  outpath,
                                  site,
                                  calibration_half_width = 14, # days
                                  filter_data = TRUE,
                                  force_cal_to_beginning = FALSE,
                                  force_cal_to_end = FALSE,
                                  r2_thres = 0.95,
                                  slope_tolerance = 9999) {
  
  # stack data available for a given site into a single timeseries.
  if (packageVersion("neonUtilities") >= "2.1.1") {
    wiso_ref <- neonUtilities::stackEddy(inpath, level = "dp01", avg = 3, var = 'isoH2o')
    wiso_amb <- neonUtilities::stackEddy(inpath, level = "dp01", avg = 9, var = 'isoH2o')  
  } else {
    wiso_ref <- neonUtilities::stackEddy(inpath, level = "dp01", avg = 3)
    wiso_amb <- neonUtilities::stackEddy(inpath, level = "dp01", avg = 9)  
  }
  
  # extract standards data.
  high <- subset(wiso_ref[[site]], wiso_ref[[site]]$verticalPosition == 'h2oHigh')
  med  <- subset(wiso_ref[[site]], wiso_ref[[site]]$verticalPosition == 'h2oMed')
  low  <- subset(wiso_ref[[site]], wiso_ref[[site]]$verticalPosition == 'h2oLow')
  
  # restructure standards data.  
  high_rs <- extract_water_calibration_data(high, standard = 'high', method = 'by_site')
  med_rs  <- extract_water_calibration_data(med, standard = 'med', method = 'by_site')
  low_rs  <- extract_water_calibration_data(low, standard = 'low', method = 'by_site')
  
  # add fix for NEON standard swap.
  low_rs <- swap_standard_isotoperatios(low_rs)
  med_rs <- swap_standard_isotoperatios(med_rs)
  high_rs <- swap_standard_isotoperatios(high_rs)
  
  # convert times in these data.frames (btime and etime) to posixct
  low_rs$btime <- convert_NEONhdf5_to_POSIXct_time(low_rs$btime)
  low_rs$etime <- convert_NEONhdf5_to_POSIXct_time(low_rs$etime)
  med_rs$btime <- convert_NEONhdf5_to_POSIXct_time(med_rs$btime)
  med_rs$etime <- convert_NEONhdf5_to_POSIXct_time(med_rs$etime)
  high_rs$btime <- convert_NEONhdf5_to_POSIXct_time(high_rs$btime)
  high_rs$etime <- convert_NEONhdf5_to_POSIXct_time(high_rs$etime)
  
  #--------------------------------------------------------------
  # Ensure same number of measurements for each standard
  #--------------------------------------------------------------
  # add group ids using run length encoding based on time differences.
  thres_hours <- as.difftime("04:00:00", # assume any time difference 
                             format = "%H:%M:%S", # > 4 hours is a new reference measurement
                             units = "mins")
  
  high_rs <- high_rs %>%
    mutate(time_diff = ifelse(.data$btime - lag(.data$btime) > thres_hours, 1, 0))
  high_rs$periods <- rleidv(high_rs, "time_diff") %/% 2
  
  med_rs <- med_rs %>%
    mutate(time_diff = ifelse(.data$btime - lag(.data$btime) > thres_hours, 1, 0))
  med_rs$periods <- rleidv(med_rs, "time_diff") %/% 2
  
  low_rs <- low_rs %>%
    mutate(time_diff = ifelse(.data$btime - lag(.data$btime) > thres_hours, 1, 0))
  low_rs$periods <- rleidv(low_rs, "time_diff") %/% 2

  high_rs <- high_rs %>%
    group_by(.data$periods) %>%
    filter(.data$d18O_meas_n > 30 | is.na(.data$d18O_meas_n)) %>%
    slice_tail(n = 3) %>%
    ungroup()
  
  med_rs <- med_rs %>%
    group_by(.data$periods) %>%
    filter(.data$d18O_meas_n > 30 | is.na(.data$d18O_meas_n)) %>%
    slice_tail(n = 3) %>%
    ungroup()
  
  low_rs <- low_rs %>%
    group_by(.data$periods) %>%
    filter(.data$d18O_meas_n > 30 | is.na(.data$d18O_meas_n)) %>%
    slice_tail(n = 3) %>%
    ungroup()
  
  #=======================================================================
  # apply calibration routines
  #=======================================================================
  # bind together, and cleanup.
  #### OMIT FOR ERROR PROPOAGATION.
  stds <- do.call(rbind, list(high_rs, med_rs, low_rs))
  
  out <- fit_water_regression(stds,
                                     calibration_half_width = calibration_half_width,
                                     slope_tolerance = slope_tolerance,
                                     r2_thres = r2_thres)
  
  var_for_h5 <- out
  
  var_for_h5$start <- convert_POSIXct_to_NEONhdf5_time(var_for_h5$start)
  var_for_h5$end <- convert_POSIXct_to_NEONhdf5_time(var_for_h5$end)
  
  var_for_h5$timeBgn <- var_for_h5$start
  var_for_h5$timeEnd <- var_for_h5$end
  
  # remove old vars.
  var_for_h5$start <- var_for_h5$end <- NULL

  #----------------------------------
  # write out to h5 file.
  #----------------------------------
  # generate file name:
  inname <- list.files(inpath, pattern = '.h5', full.names = TRUE)[[1]]
  inname_list <- strsplit(inname, split = '.', fixed = TRUE)
  
  outname <- paste0(outpath,"/NEON.",inname_list[[1]][2],'.',site,".DP4.00200.001.nsae.all.basic.wiso.calibrated.",
                    2*calibration_half_width,"dayWindow.h5")
  
  setup_output_file(inname, outname, site, analyte = 'h2o')
  
  # okay try to write out to h5 file.
  write_water_calibration_data(outname, site, var_for_h5)
  
  # copy over objDesk and readme
  tmp <- rhdf5::h5read(inname, '/objDesc')
  rhdf5::h5write(tmp, file = outname, '/objDesc')
  
  tmp <- rhdf5::h5read(inname, '/readMe')
  rhdf5::h5write(tmp, file = outname, '/readMe')
  # #---------------------------------------------
  # #---------------------------------------------
  # # copy high/mid/low standard data from input file.
  # #---------------------------------------------
  # #---------------------------------------------
  write_water_reference_data(inname, outname, site,
                             lowDf = low,
                             medDf = med,
                             highDf = high,
                             calDf = out)
  
  Sys.sleep(0.5)
  
  rhdf5::h5closeAll()
  # 
  #===========================================================
  # calibrate data for each height.
  #-------------------------------------
  # stack data to get ambient observations.
  print("stacking ambient data...this may take a while...")
  
  data_by_height_by_var <- restructure_ambient_data(inpath, 'H2o')
  
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
  
}
