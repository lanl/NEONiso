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
                                          force_cal_to_beginning = FALSE,
                                          force_cal_to_end = FALSE,
                                          r2_thres = 0.95,
                                          slope_tolerance = 9999) {
  
  # stack data available for a given site into a single timeseries.
  wiso_ref <- neonUtilities::stackEddy(inpath, level = "dp01", avg = 3)
  wiso_amb <- neonUtilities::stackEddy(inpath, level = "dp01", avg = 9)
  
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
  fid <- rhdf5::H5Fopen(outname)
  
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
  # #low
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))
  
  low_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))
  
  # restructure variables to be more suitable for output file.
  dlta18OH2o           <- restructure_water_variables(low, "dlta18OH2o", "reference")
  dlta2HH2o            <- restructure_water_variables(low, "dlta2HH2o", "reference")
  dlta18OH2oRefe       <- restructure_water_variables(low, "dlta18OH2oRefe", "reference")
  dlta2HH2oRefe        <- restructure_water_variables(low, "dlta2HH2oRefe", "reference")
  pres                 <- restructure_water_variables(low, "pres", "reference")
  presEnvHut           <- restructure_water_variables(low, "presEnvHut", "reference")
  rhEnvHut             <- restructure_water_variables(low, "rhEnvHut", "reference")
  rtioMoleWetH2o       <- restructure_water_variables(low, "rtioMoleWetH2o", "reference")
  rtioMoleWetH2oEnvHut <- restructure_water_variables(low, "rtioMoleWetH2oEnvHut", "reference")
  temp                 <- restructure_water_variables(low, "temp", "reference")
  tempEnvHut           <- restructure_water_variables(low, "tempEnvHut", "reference")
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[1]], dlta2HH2o[[1]], dlta18OH2oRefe[[1]], dlta2HH2oRefe[[1]],
                                     pres[[1]], presEnvHut[[1]], rhEnvHut[[1]],
                                     rtioMoleWetH2o[[1]], rtioMoleWetH2oEnvHut[[1]], temp[[1]], tempEnvHut[[1]]))
  
  lowref <- base::split(data_out_all, factor(data_out_all$varname))
  
  lowref <- calibrate_standards_water(out, lowref)
  
  # and write out as a dataframe.
  lapply(names(lowref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = lowref[[x]],
                                     h5loc = low_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(low_outloc)
  
  # write qfqm
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/isoH2o/h2olow_03m"))
  
  low_outloc <- rhdf5::H5Gopen(fid,
                             paste0("/", site, "/dp01/qfqm/isoH2o/h2olow_03m"))
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[2]], dlta2HH2o[[2]],
                                     pres[[2]], presEnvHut[[2]], rhEnvHut[[2]],
                                     rtioMoleWetH2o[[2]], rtioMoleWetH2oEnvHut[[2]], temp[[2]], tempEnvHut[[2]]))
  
  lowref <- base::split(data_out_all, factor(data_out_all$varname))
  
  # and write out as a dataframe.
  lapply(names(lowref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = lowref[[x]],
                                     h5loc = low_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(low_outloc)
  
  # write ucrt 
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/isoH2o/h2olow_03m"))
  
  low_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/ucrt/isoH2o/h2olow_03m"))
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[3]], dlta2HH2o[[3]],
                                     pres[[3]], presEnvHut[[3]], rhEnvHut[[3]],
                                     rtioMoleWetH2o[[3]], rtioMoleWetH2oEnvHut[[3]], temp[[3]], tempEnvHut[[3]]))
  
  lowref <- base::split(data_out_all, factor(data_out_all$varname))
  
  # and write out as a dataframe.
  lapply(names(lowref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = lowref[[x]],
                                     h5loc = low_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(low_outloc)
  
  # #------------------------------------------------------------
  # #medium
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))
  
  med_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))
  
  # restructure variables to be more suitable for output file.
  dlta18OH2o           <- restructure_water_variables(med, "dlta18OH2o", "reference")
  dlta2HH2o            <- restructure_water_variables(med, "dlta2HH2o", "reference")
  dlta18OH2oRefe       <- restructure_water_variables(med, "dlta18OH2oRefe", "reference")
  dlta2HH2oRefe        <- restructure_water_variables(med, "dlta2HH2oRefe", "reference")
  pres                 <- restructure_water_variables(med, "pres", "reference")
  presEnvHut           <- restructure_water_variables(med, "presEnvHut", "reference")
  rhEnvHut             <- restructure_water_variables(med, "rhEnvHut", "reference")
  rtioMoleWetH2o       <- restructure_water_variables(med, "rtioMoleWetH2o", "reference")
  rtioMoleWetH2oEnvHut <- restructure_water_variables(med, "rtioMoleWetH2oEnvHut", "reference")
  temp                 <- restructure_water_variables(med, "temp", "reference")
  tempEnvHut           <- restructure_water_variables(med, "tempEnvHut", "reference")
  
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[1]], dlta2HH2o[[1]], dlta18OH2oRefe[[1]], dlta2HH2oRefe[[1]],
                                     pres[[1]], presEnvHut[[1]], rhEnvHut[[1]],
                                     rtioMoleWetH2o[[1]], rtioMoleWetH2oEnvHut[[1]], temp[[1]], tempEnvHut[[1]]))
  
  medref <- base::split(data_out_all, factor(data_out_all$varname))
  
  medref <- calibrate_standards_water(out, medref)
  
  # and write out as a dataframe.
  lapply(names(medref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = medref[[x]],
                                     h5loc = med_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(med_outloc)
  
  # write qfqm
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/isoH2o/h2omed_03m"))
  
  med_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/qfqm/isoH2o/h2omed_03m"))
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[2]], dlta2HH2o[[2]],
                                     pres[[2]], presEnvHut[[2]], rhEnvHut[[2]],
                                     rtioMoleWetH2o[[2]], rtioMoleWetH2oEnvHut[[2]], temp[[2]], tempEnvHut[[2]]))
  
  medref <- base::split(data_out_all, factor(data_out_all$varname))
  
  # and write out as a dataframe.
  lapply(names(medref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = medref[[x]],
                                     h5loc = med_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(med_outloc)
  
  # write ucrt 
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/isoH2o/h2omed_03m"))
  
  med_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/ucrt/isoH2o/h2omed_03m"))
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[3]], dlta2HH2o[[3]],
                                     pres[[3]], presEnvHut[[3]], rhEnvHut[[3]],
                                     rtioMoleWetH2o[[3]], rtioMoleWetH2oEnvHut[[3]], temp[[3]], tempEnvHut[[3]]))
  
  medref <- base::split(data_out_all, factor(data_out_all$varname))
  
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
  
  # restructure variables to be more suitable for output file.
  dlta18OH2o           <- restructure_water_variables(high, "dlta18OH2o", "reference")
  dlta2HH2o            <- restructure_water_variables(high, "dlta2HH2o", "reference")
  dlta18OH2oRefe       <- restructure_water_variables(high, "dlta18OH2oRefe", "reference")
  dlta2HH2oRefe        <- restructure_water_variables(high, "dlta2HH2oRefe", "reference")
  pres                 <- restructure_water_variables(high, "pres", "reference")
  presEnvHut           <- restructure_water_variables(high, "presEnvHut", "reference")
  rhEnvHut             <- restructure_water_variables(high, "rhEnvHut", "reference")
  rtioMoleWetH2o       <- restructure_water_variables(high, "rtioMoleWetH2o", "reference")
  rtioMoleWetH2oEnvHut <- restructure_water_variables(high, "rtioMoleWetH2oEnvHut", "reference")
  temp                 <- restructure_water_variables(high, "temp", "reference")
  tempEnvHut           <- restructure_water_variables(high, "tempEnvHut", "reference")  
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[1]], dlta2HH2o[[1]], dlta18OH2oRefe[[1]], dlta2HH2oRefe[[1]],
                                     pres[[1]], presEnvHut[[1]], rhEnvHut[[1]],
                                     rtioMoleWetH2o[[1]], rtioMoleWetH2oEnvHut[[1]], temp[[1]], tempEnvHut[[1]]))
  
  highref <- base::split(data_out_all, factor(data_out_all$varname))
  
  highref <- calibrate_standards_water(out, highref)
  
  # and write out as a dataframe.
  lapply(names(highref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = highref[[x]],
                                     h5loc = high_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(high_outloc)
  
  # write qfqm
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/isoH2o/h2ohigh_03m"))
  
  high_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/qfqm/isoH2o/h2ohigh_03m"))
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[2]], dlta2HH2o[[2]],
                                     pres[[2]], presEnvHut[[2]], rhEnvHut[[2]],
                                     rtioMoleWetH2o[[2]], rtioMoleWetH2oEnvHut[[2]], temp[[2]], tempEnvHut[[2]]))
  
  highref <- base::split(data_out_all, factor(data_out_all$varname))
  
  # and write out as a dataframe.
  lapply(names(highref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = highref[[x]],
                                     h5loc = high_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(high_outloc)
  
  # write ucrt 
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/isoH2o/h2ohigh_03m"))
  
  high_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/ucrt/isoH2o/h2ohigh_03m"))
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[3]], dlta2HH2o[[3]],
                                     pres[[3]], presEnvHut[[3]], rhEnvHut[[3]],
                                     rtioMoleWetH2o[[3]], rtioMoleWetH2oEnvHut[[3]], temp[[3]], tempEnvHut[[3]]))
  
  highref <- base::split(data_out_all, factor(data_out_all$varname))
  
  # and write out as a dataframe.
  lapply(names(highref), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = highref[[x]],
                                     h5loc = high_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(high_outloc)

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
  dlta18OH2o <- restructure_water_variables(dlta18O_list, "dlta18OH2o", "ambient")
  
  dlta2H_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "dlta2HH2o", avg = 9)
  dlta2HH2o <- restructure_water_variables(dlta2H_list, "dlta2HH2o", "ambient")
  
  pres_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "pres", avg = 9)
  pres <- restructure_water_variables(pres_list, "pres", "ambient")
  
  presEnvHut_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "presEnvHut", avg = 9)
  presEnvHut <- restructure_water_variables(presEnvHut_list, "presEnvHut", "ambient")
  
  rhEnvHut_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "rhEnvHut", avg = 9)
  rhEnvHut <- restructure_water_variables(rhEnvHut_list, "rhEnvHut", "ambient")
  
  rtioMoleWetH2o_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "rtioMoleWetH2o", avg = 9)
  rtioMoleWetH2o <- restructure_water_variables(rtioMoleWetH2o_list, "rtioMoleWetH2o", "ambient")
  
  rtioMoleWetH2oEnvHut_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "rtioMoleWetH2oEnvHut", avg = 9)
  rtioMoleWetH2oEnvHut <- restructure_water_variables(rtioMoleWetH2oEnvHut_list, "rtioMoleWetH2oEnvHut", "ambient")
  
  temp_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "temp", avg = 9)
  temp <- restructure_water_variables(temp_list, "temp", "ambient")
  
  tempEnvHut_list <- neonUtilities::stackEddy(inpath, level = "dp01", var = "tempEnvHut", avg = 9)
  tempEnvHut <- restructure_water_variables(tempEnvHut_list, "tempEnvHut", "ambient")
  
  data_out_all <- do.call(rbind,list(dlta18OH2o[[1]], dlta2HH2o[[1]], pres[[1]], presEnvHut[[1]], rhEnvHut[[1]],
                                     rtioMoleWetH2o[[1]], rtioMoleWetH2oEnvHut[[1]], temp[[1]], tempEnvHut[[1]]))
  
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
  
}