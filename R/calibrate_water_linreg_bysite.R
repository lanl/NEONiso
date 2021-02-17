#' calibrate_water_linreg
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
calibrate_water_linreg_bysite <- function(inpath,
                                   outpath,
                                   site,
                                   filter_data = TRUE,
                                   force_cal_to_beginning = TRUE,
                                   force_cal_to_end = TRUE,
                                   r2_thres = 0.95) {

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
  # this section needs to be refactored: had originally assumed there was
  # only one month. need to restructure to be able to cross years.
  # there's an additional problem here: what happens if standards cross midnight??
  
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
    
    # assign a vector corresponding to calibration period.
    # stds$cal_period <- stds$d18O_meas_n
    
    # loop through days represented in the dataframe,
    # select rows corresponding to window of interest, 
    # and run regression.
    
  # 
  #   # okay, now run calibrations...
  #   #------------------------------
  # 
  #   # create output variables.
  #   oxy_cal_slopes <- vector()
  #   oxy_cal_ints   <- vector()
  #   oxy_cal_rsq    <- vector()
  # 
  #   hyd_cal_slopes <- vector()
  #   hyd_cal_ints   <- vector()
  #   hyd_cal_rsq    <- vector()
  # 
  #   for (i in 2:max(stds$cal_period)) {
  #     # check to see if data exist.
  #     cal_subset <- stds[which(stds$cal_period == i |
  #                                stds$cal_period == (i - 1)), ]
  # 
  #     # check to see if sum of is.na() on oxygen data = nrow of oxygen data
  #     if (sum(is.na(cal_subset$d18O_meas_mean)) < nrow(cal_subset) &
  #         sum(is.na(cal_subset$d18O_ref_mean)) < nrow(cal_subset)) {
  #       tmp <- lm(d18O_ref_mean ~ d18O_meas_mean, data = cal_subset)
  # 
  #       oxy_cal_slopes[i - 1] <- coef(tmp)[[2]]
  #       oxy_cal_ints[i - 1] <- coef(tmp)[[1]]
  #       oxy_cal_rsq[i - 1] <- summary(tmp)$r.squared
  # 
  #     } else { # all are missing
  #       oxy_cal_slopes[i - 1] <- NA
  #       oxy_cal_ints[i - 1] <- NA
  #       oxy_cal_rsq[i - 1] <- NA
  #     }
  # 
  #     # HYDROGEN
  # 
  #     # check to see if sum of is.na() on oxygen data = nrow of oxygen data
  #     if (sum(is.na(cal_subset$d2H_meas_mean)) < nrow(cal_subset) &
  #         sum(is.na(cal_subset$d2H_ref_mean)) < nrow(cal_subset)) {
  #       tmp <- lm(d2H_ref_mean ~ d2H_meas_mean, data = cal_subset)
  # 
  #       hyd_cal_slopes[i - 1] <- coef(tmp)[[2]]
  #       hyd_cal_ints[i - 1] <- coef(tmp)[[1]]
  #       hyd_cal_rsq[i - 1] <- summary(tmp)$r.squared
  # 
  #     } else { # all are missing
  #       hyd_cal_slopes[i - 1] <- NA
  #       hyd_cal_ints[i - 1] <- NA
  #       hyd_cal_rsq[i - 1] <- NA
  #     }
  #   }
  # 
  #   # make dataframe of calibration data.
  #   times <- stds %>%
  #     select(btime, etime, cal_period) %>%
  #     group_by(cal_period) %>%
  #     summarize(etime = max(etime))
  # 
  #   # loop through times, assign beginning, ending value.
  #   # max etime should be just fine.
  #   starttimes <- vector()
  #   endtimes <- vector()
  # 
  #   for (i in 1:length(oxy_cal_slopes)) {
  #     starttimes[i] <- times$etime[i]
  #     endtimes[i] <- times$etime[i + 1]
  #   }
  # 
  #   # output dataframe giving valid time range, slopes, intercepts, rsquared.
  #   out <- data.frame(start = as.POSIXct(starttimes,
  #                                        tz = "UTC",
  #                                        origin = "1970-01-01"),
  #                     end = as.POSIXct(endtimes,
  #                                      tz = "UTC",
  #                                      origin = "1970-01-01"),
  #                     o_slope = as.numeric(oxy_cal_slopes),
  #                     o_intercept = as.numeric(oxy_cal_ints),
  #                     o_r2 = as.numeric(oxy_cal_rsq),
  #                     h_slope = as.numeric(hyd_cal_slopes),
  #                     h_intercept = as.numeric(hyd_cal_ints),
  #                     h_r2 = as.numeric(hyd_cal_rsq))
  # 
  # } else {
  #   out <- data.frame(start = as.POSIXct(starttimes,
  #                                        tz = "UTC",
  #                                        origin = "1970-01-01"),
  #                     end = as.POSIXct(endtimes,
  #                                      tz = "UTC",
  #                                      origin = "1970-01-01"),
  #                     o_slope = as.numeric(NA),
  #                     o_intercept = as.numeric(NA),
  #                     o_r2 = as.numeric(NA),
  #                     h_slope = as.numeric(NA),
  #                     h_intercept = as.numeric(NA),
  #                     h_r2 = as.numeric(NA))
 # }
  #--------------------------------------------------------------------
  # perform interpolation, if requested.
  # if (interpolate.missing.cals == TRUE) {
  #
  #   # need to filter out poor values.
  #   out$o.slope[out$o.r2 < 0.9] <- NA
  #   out$o.intercept[out$o.r2 < 0.9] <- NA
  #   out$o.r2[out$o.r2 < 0.9] <- NA
  #   out$h.slope[out$h.r2 < 0.9] <- NA
  #   out$h.intercept[out$h.r2 < 0.9] <- NA
  #   out$h.slope[out$h.r2 < 0.9] <- NA
  #
  #   if (sum(!is.na(out$o.slope)) > 5 & sum(!is.na(out$o.slope)) > 5) {
  #
  #     # check to determine which method to use.
  #     if (interpolation.method == "LWMA") {
  #       # save a vector of which values have been replaced!
  #       replaced.vals <- ifelse(is.na(out$o.slope), 1, 0)
  #
  #       print(paste0(100*sum(replaced.vals)/length(replaced.vals), "% of values filled w/ LWMA"))
  #
  #       # linear weighted moving average chosen.
  #       out$o.slope <- imputeTS::na_ma(out$o.slope,  weighting = "linear")
  #       out$h.slope <- imputeTS::na_ma(out$h.slope,  weighting = "linear")
  #       out$o.intercept <- imputeTS::na_ma(out$o.intercept,  weighting = "linear")
  #       out$h.intercept <- imputeTS::na_ma(out$h.intercept,  weighting = "linear")
  #
  #     } else if (interpolation.method == "LOCF") {
  #
  #       stop("LOCF not activated yet.")
  #     } else {
  #       stop("Interpolation method not recognized. Valid values currently are LOCF or LWMA, others to come if requested.")
  #     }
  #   } else {
  #     # set replaced.vals as 0, since none were replaced.
  #     print("Too many values are missing, so do not interpolate...")
  #     replaced.vals <- rep(0,nrow(out))
  #   }
  # }
# 
#   # check to ensure there are 6 columns.
#   # add slope, intercept, r2 columns if missing.
#   if (!("o_slope" %in% names(out))) {
#     out$o_slope <- as.numeric(rep(NA, length(out$start)))
#   }
#   if (!("o_intercept" %in% names(out))) {
#     out$o_intercept <- as.numeric(rep(NA, length(out$start)))
#   }
#   if (!("o_r2" %in% names(out))) {
#     out$o_r2 <- as.numeric(rep(NA, length(out$start)))
#   }
#   if (!("h_slope" %in% names(out))) {
#     out$h_slope <- as.numeric(rep(NA, length(out$start)))
#   }
#   if (!("h_intercept" %in% names(out))) {
#     out$h_intercept <- as.numeric(rep(NA, length(out$start)))
#   }
#   if (!("h_r2" %in% names(out))) {
#     out$h_r2 <- as.numeric(rep(NA, length(out$start)))
#   }
# 
# 
#   # ensure there are 8 columns in out!
#   if (ncol(out) != 8) {
#     stop("Wrong number of columns in out.")
#   }
# 
#   var_for_h5 <- out
#   
#   print(var_for_h5)
#   # 
  # var_for_h5$start <- convert_POSIXct_to_NEONhdf5_time(var_for_h5$start)
  # 
  # var_for_h5$end <- convert_POSIXct_to_NEONhdf5_time(var_for_h5$end)
  # 
  # var_for_h5$valid_period_start <- var_for_h5$start
  # var_for_h5$valid_period_end   <- var_for_h5$end
  # 
  # # remove old vars.
  # var_for_h5$start <- var_for_h5$end <- NULL
  # 
  # # okay try to write out to h5 file.
  # rhdf5::h5createFile(outname)
  # rhdf5::h5createGroup(outname, paste0("/", site))
  # rhdf5::h5createGroup(outname, paste0("/", site, "/dp01"))
  # rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data"))
  # rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/isoH2o"))
  # 
  # # okay try to write out to h5 file.
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
  # rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/isoH2o/calData"))
  # 
  # h2o_cal_outloc <- rhdf5::H5Gopen(fid,
  #                             paste0("/", site, "/dp01/data/isoH2o/calData"))
  # 
  # # write out dataset.
  # rhdf5::h5writeDataset.data.frame(obj = var_for_h5,
  #                                  h5loc = h2o_cal_outloc,
  #                                  name = "calRegressions",
  #                                  DataFrameAsCompound = TRUE)
  # 
  # # close the group and the file
  # rhdf5::H5Gclose(h2o_cal_outloc)
  # 
  # #---------------------------------------------
  # #---------------------------------------------
  # # copy high/mid/low standard data from input file.
  # #---------------------------------------------
  # #---------------------------------------------
  # #low
  # rhdf5::h5createGroup(outname,
  #                      paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))
  # 
  # low_outloc <- rhdf5::H5Gopen(fid,
  #                          paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))
  # 
  # low <- rhdf5::h5read(inname,
  #                      paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))
  # 
  # low <- calibrate_standards_water(out, low)
  # 
  # # loop through each of the variables in list amb.data.list
  # # and write out as a dataframe.
  # lapply(names(low), function(x) {
  #   rhdf5::h5writeDataset.data.frame(obj = low[[x]],
  #                                    h5loc = low_outloc,
  #                                    name = x,
  #                                    DataFrameAsCompound = TRUE)})
  # 
  # rhdf5::H5Gclose(low_outloc)
  # 
  # #------------------------------------------------------------
  # #medium
  # rhdf5::h5createGroup(outname,
  #                      paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))
  # 
  # med_outloc <- rhdf5::H5Gopen(fid,
  #                       paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))
  # 
  # med <- rhdf5::h5read(inname,
  #                      paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))
  # 
  # med <- calibrate_standards_water(out, med)
  # 
  # # loop through each of the variables in list amb.data.list
  # # and write out as a dataframe.
  # lapply(names(med), function(x) {
  #   rhdf5::h5writeDataset.data.frame(obj = med[[x]],
  #                                    h5loc = med_outloc,
  #                                    name = x,
  #                                    DataFrameAsCompound = TRUE)})
  # 
  # rhdf5::H5Gclose(med_outloc)
  # 
  # #------------------------------------------------------------
  # #high
  # rhdf5::h5createGroup(outname,
  #                      paste0("/", site, "/dp01/data/isoH2o/h2oHigh_03m"))
  # 
  # high_outloc <- rhdf5::H5Gopen(fid,
  #                      paste0("/", site, "/dp01/data/isoH2o/h2oHigh_03m"))
  # 
  # high <- rhdf5::h5read(inname,
  #                       paste0("/", site, "/dp01/data/isoH2o/h2oHigh_03m"))
  # 
  # high <- calibrate_standards_water(out, high)
  # 
  # # loop through each of the variables in list amb.data.list
  # # and write out as a dataframe.
  # lapply(names(high), function(x) {
  #   rhdf5::h5writeDataset.data.frame(obj = high[[x]],
  #                                    h5loc = high_outloc,
  #                                    name = x,
  #                                    DataFrameAsCompound = TRUE)})
  # 
  # rhdf5::H5Gclose(high_outloc)
  # 
  # # close the group and the file
  # rhdf5::H5Fclose(fid)
  # Sys.sleep(0.5)
  # 
  # rhdf5::h5closeAll()
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
  # #===========================================================
  # # calibrate data for each height.
  # #-------------------------------------
  # # extract ambient measurements from ciso
  # wiso_logical <- grepl(pattern = "000", x = names(wiso))
  # wiso_subset <- wiso[wiso_logical]
  # 
  # lapply(names(wiso_subset),
  #        function(x) {
  #          calibrate_ambient_water_linreg(amb_data_list = wiso_subset[[x]],
  #                                                   caldf = out,
  #                                                   outname = x,
  #                                                   file = outname,
  #                                                   site = site,
  #                                                   filter_data = filter_data,
  #                                                   force_to_end = force_cal_to_end,
  #                                                   force_to_beginning = force_cal_to_beginning,
  #                                                   r2_thres = r2_thres)})
  # 
  # rhdf5::h5closeAll()
  # 
  # 
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
