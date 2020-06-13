#' calibrate_water_linreg
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param site Four-letter NEON code for site being processed.
#' @param time_diff_betweeen_standards Time (in seconds) required between consecutive standard measurements.
#' @param inname Name of the input file.
#' @param outname Name of the output file.
#' @param force_cal_to_beginning Extend first calibration to
#'                               the beginning of the file?
#' @param force_cal_to_end Extend last calibration to the end of the file?
#' @param r2_thres Minimum r2 threshold of an "acceptable" calibration. Acts to
#'            remove calibration periods where a measurement error makes
#'            relationship nonlinear. Default = 0.95
#'
#' @return nothing to the workspace, but creates a new output file of
#'         calibrated carbon isotope data.
#' @export
#'
#' @examples
#' 
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%
calibrate_water_linreg <- function(inname,
                                   outname,
                                   site,
                                   time_diff_betweeen_standards = 1800,
                                   force_cal_to_beginning = TRUE,
                                   force_cal_to_end = TRUE,
                                   r2_thres = 0.95) {
  
  # print status.
  print("Processing water calibration data...")
  
  # load file, get calibration data.
  wiso <- rhdf5::h5read(inname, paste0("/", site, "/dp01/data/isoH2o"))
  
  # extract standards data.
  high <- wiso$h2oHigh_03m
  med <- wiso$h2oMed_03m
  low <- wiso$h2oLow_03m
  
  # attempt to pull relevent data out to a single dataframe.
  high_rs <- data.frame(d18O_meas_mean = high$dlta18OH2o$mean, 
                        d18O_meas_var = high$dlta18OH2o$vari,
                        d18O_meas_n = high$dlta18OH2o$numSamp,
                        d18O_meas_btime = high$dlta18OH2o$timeBgn,
                        d18O_meas_etime = high$dlta18OH2o$timeEnd,
                        d18O_ref_mean = high$dlta18OH2oRefe$mean,
                        d18O_ref_var = high$dlta18OH2oRefe$vari,
                        d18O_ref_n = high$dlta18OH2oRefe$numSamp,
                        d18O_ref_btime = high$dlta18OH2oRefe$timeBgn,
                        d18O_ref_etime = high$dlta18OH2oRefe$timeEnd,
                        d2H_meas_mean = high$dlta2HH2o$mean,
                        d2H_meas_var = high$dlta2HH2o$vari,
                        d2H_meas_n = high$dlta2HH2o$numSamp,
                        d2H_meas_btime = high$dlta2HH2o$timeBgn,
                        d2H_meas_etime = high$dlta2HH2o$timeEnd,
                        d2H_ref_mean = high$dlta2HH2oRefe$mean,
                        d2H_ref_var = high$dlta2HH2oRefe$vari,
                        d2H_ref_n = high$dlta2HH2oRefe$numSamp,
                        d2H_ref_btime = high$dlta2HH2oRefe$timeBgn,
                        d2H_ref_etime = high$dlta2HH2oRefe$timeEnd)
  
  high_rs <- high_rs %>%
    mutate(std_name = "high")
  
  med_rs <- data.frame(d18O_meas_mean = med$dlta18OH2o$mean,
                       d18O_meas_var = med$dlta18OH2o$vari,
                       d18O_meas_n = med$dlta18OH2o$numSamp,
                       d18O_meas_btime = med$dlta18OH2o$timeBgn,
                       d18O_meas_etime = med$dlta18OH2o$timeEnd,
                       d18O_ref_mean = med$dlta18OH2oRefe$mean,
                       d18O_ref_var = med$dlta18OH2oRefe$vari,
                       d18O_ref_n = med$dlta18OH2oRefe$numSamp,
                       d18O_ref_btime = med$dlta18OH2oRefe$timeBgn,
                       d18O_ref_etime = med$dlta18OH2oRefe$timeEnd,
                       d2H_meas_mean = med$dlta2HH2o$mean,
                       d2H_meas_var = med$dlta2HH2o$vari,
                       d2H_meas_n = med$dlta2HH2o$numSamp,
                       d2H_meas_btime = med$dlta2HH2o$timeBgn,
                       d2H_meas_etime = med$dlta2HH2o$timeEnd,
                       d2H_ref_mean = med$dlta2HH2oRefe$mean,
                       d2H_ref_var = med$dlta2HH2oRefe$vari,
                       d2H_ref_n = med$dlta2HH2oRefe$numSamp,
                       d2H_ref_btime = med$dlta2HH2oRefe$timeBgn,
                       d2H_ref_etime = med$dlta2HH2oRefe$timeEnd)

  med_rs <- med_rs %>%
    mutate(std_name = "med")

  low_rs <- data.frame(d18O_meas_mean = low$dlta18OH2o$mean,
                       d18O_meas_var = low$dlta18OH2o$vari,
                       d18O_meas_n = low$dlta18OH2o$numSamp,
                       d18O_meas_btime = low$dlta18OH2o$timeBgn,
                       d18O_meas_etime = low$dlta18OH2o$timeEnd,
                       d18O_ref_mean = low$dlta18OH2oRefe$mean,
                       d18O_ref_var = low$dlta18OH2oRefe$vari,
                       d18O_ref_n = low$dlta18OH2oRefe$numSamp,
                       d18O_ref_btime = low$dlta18OH2oRefe$timeBgn,
                       d18O_ref_etime = low$dlta18OH2oRefe$timeEnd,
                       d2H_meas_mean = low$dlta2HH2o$mean,
                       d2H_meas_var = low$dlta2HH2o$vari,
                       d2H_meas_n = low$dlta2HH2o$numSamp,
                       d2H_meas_btime = low$dlta2HH2o$timeBgn,
                       d2H_meas_etime = low$dlta2HH2o$timeEnd,
                       d2H_ref_mean = low$dlta2HH2oRefe$mean,
                       d2H_ref_var = low$dlta2HH2oRefe$vari,
                       d2H_ref_n = low$dlta2HH2oRefe$numSamp,
                       d2H_ref_btime = low$dlta2HH2oRefe$timeBgn,
                       d2H_ref_etime = low$dlta2HH2oRefe$timeEnd)

  low_rs <- low_rs %>%
    mutate(std_name = "low")

  # add fix for NEON standard swap.
  low_rs <- swap_standard_isotoperatios(low_rs)

  #--------------------------------------------------------------
  # Ensure there are the same number of standard measurements for each standard.
  #--------------------------------------------------------------
  
  # 191024 rpf - prior versions of this have just sliced out the first observation per day.
  # however, the most common cause of multiple standards to be analyzed per day is a
  # malfunctioning valve in the manifold that causes the same standard gas to register as multiple
  # peaks. each peak is shorter, higher variance, and doesn't allow even the CO2 concentration
  # to stabilize. until further notice, i suggest removing these standards altogether.
  # code below has been modified to achieve this.
  # 200103 rpf - copying over this code from carbon script to fix the same bug present in
  # the water isotope code. modify slightly to account for the fact that we expect more than
  # 1 row per day. commented out 

  high_rs <- high_rs %>%
    mutate(dom = day(d18O_meas_btime)) %>% # get day of month
    group_by(dom) %>%
    filter(d18O_meas_n > 30 | is.na(d18O_meas_n)) %>% # check to make sure peak sufficiently long, then slice off single.
    slice(tail(row_number(), 3)) %>%
    ungroup()

  med_rs <- med_rs %>%
    mutate(dom = day(d18O_meas_btime)) %>% # get day of month
    group_by(dom) %>%
    filter(d18O_meas_n > 30 | is.na(d18O_meas_n)) %>% # check to make sure peak sufficiently long, then slice off single.
    slice(tail(row_number(), 3)) %>%
    ungroup()

  low_rs <- low_rs %>%
    mutate(dom = day(d18O_meas_btime)) %>% # get day of month
    group_by(dom) %>%
    filter(d18O_meas_n > 30 | is.na(d18O_meas_n)) %>% # check to make sure peak sufficiently long, then slice off single.
    slice(tail(row_number(), 3)) %>%
    ungroup()

  #=======================================================================
  # apply calibration routines
  #=======================================================================
  # bind together, and cleanup.
  #### OMIT FOR ERROR PROPOAGATION.
  stds <- do.call(rbind, list(high_rs, med_rs, low_rs))

  if (nrow(stds) > 0) {
    # replace NaNs with NA
    # rpf note on 181121 - what does this line actually do? Seems tautological.
    # rpf note 181126 - is.na() also returns NaN as NA, so this does actually do what first
    # comment indicates.
    stds[is.na(stds)] <- NA

    #-----------------------------------------------------------
    # CALIBRATE WATER ISOTOPE VALUES

    # change class of time variables from charatcter to posixct.
    stds$d18O_meas_btime <- as.POSIXct(stds$d18O_meas_btime,
                                       format = "%Y-%m-%dT%H:%M:%OSZ",
                                       tz = "UTC")
    stds$d18O_meas_etime <- as.POSIXct(stds$d18O_meas_etime,
                                       format = "%Y-%m-%dT%H:%M:%OSZ",
                                       tz = "UTC")

    stds$d18O_ref_btime <- as.POSIXct(stds$d18O_ref_btime,
                                      format = "%Y-%m-%dT%H:%M:%OSZ",
                                      tz = "UTC")
    stds$d18O_ref_etime <- as.POSIXct(stds$d18O_ref_etime,
                                      format = "%Y-%m-%dT%H:%M:%OSZ",
                                      tz = "UTC")

    stds$d2H_meas_btime <- as.POSIXct(stds$d2H_meas_btime,
                                      format = "%Y-%m-%dT%H:%M:%OSZ",
                                      tz = "UTC")
    stds$d2H_meas_etime <- as.POSIXct(stds$d2H_meas_etime,
                                      format = "%Y-%m-%dT%H:%M:%OSZ",
                                      tz = "UTC")

    stds$d2H_ref_btime <- as.POSIXct(stds$d2H_ref_btime,
                                     format = "%Y-%m-%dT%H:%M:%OSZ",
                                     tz = "UTC")
    stds$d2H_ref_etime <- as.POSIXct(stds$d2H_ref_etime,
                                     format = "%Y-%m-%dT%H:%M:%OSZ",
                                     tz = "UTC")

    # reorder data frame
    stds <- stds[order(stds$d18O_meas_btime), ]

    # assign a vector corresponding to calibration period.
    stds$cal_period <- stds$d18O_meas_n

    period_id <- 1
    tdiffs <- c(diff(stds$d18O_meas_btime), 0)
    for (i in 1:nrow(stds)) {
      stds$cal_period[i] <- period_id

      if (tdiffs[i] >= time_diff_betweeen_standards) {
        period_id = period_id + 1
      }
    }

    # okay, now run calibrations...
    #------------------------------

    # create output variables.
    oxy_cal_slopes <- vector()
    oxy_cal_ints   <- vector()
    oxy_cal_rsq    <- vector()

    hyd_cal_slopes <- vector()
    hyd_cal_ints   <- vector()
    hyd_cal_rsq    <- vector()

    for (i in 2:max(stds$cal_period)) {
      # check to see if data exist.
      cal_subset <- stds[which(stds$cal_period == i | stds$cal_period == (i - 1)), ]

      # check to see if sum of is.na() on oxygen data = nrow of oxygen data
      if (sum(is.na(cal_subset$d18O_meas_mean)) < nrow(cal_subset) &
          sum(is.na(cal_subset$d18O_ref_mean)) < nrow(cal_subset)) {
        tmp <- lm(d18O_ref_mean ~ d18O_meas_mean, data = cal_subset)

        oxy_cal_slopes[i - 1] <- coef(tmp)[[2]]
        oxy_cal_ints[i - 1] <- coef(tmp)[[1]]
        oxy_cal_rsq[i - 1] <- summary(tmp)$r.squared

      } else { # all are missing
        oxy_cal_slopes[i - 1] <- NA
        oxy_cal_ints[i - 1] <- NA
        oxy_cal_rsq[i - 1] <- NA
      }

      # HYDROGEN

      # check to see if sum of is.na() on oxygen data = nrow of oxygen data
      if (sum(is.na(cal_subset$d2H_meas_mean)) < nrow(cal_subset) &
          sum(is.na(cal_subset$d2H_ref_mean)) < nrow(cal_subset)) {
        tmp <- lm(d2H_ref_mean ~ d2H_meas_mean, data = cal_subset)

        hyd_cal_slopes[i - 1] <- coef(tmp)[[2]]
        hyd_cal_ints[i - 1] <- coef(tmp)[[1]]
        hyd_cal_rsq[i - 1] <- summary(tmp)$r.squared  

      } else { # all are missing
        hyd_cal_slopes[i - 1] <- NA
        hyd_cal_ints[i - 1] <- NA
        hyd_cal_rsq[i - 1] <- NA 
      }
    }

    # make dataframe of calibration data.
    times <- stds %>%
      select(d18O_meas_btime, d18O_meas_etime, d18O_ref_btime, d18O_ref_etime,
             d2H_meas_btime, d2H_meas_etime, d2H_ref_btime, d2H_ref_etime, cal_period) %>%
      group_by(cal_period) %>%
      summarize(etime = max(c(d18O_meas_etime, d18O_ref_etime, d2H_meas_etime, d2H_ref_etime)))

    # loop through times, assign beginning, ending value. max etime should be just fine.
    starttimes <- vector()
    endtimes <- vector()

    for (i in 1:length(oxy_cal_slopes)) {
      starttimes[i] <- times$etime[i]
      endtimes[i] <- times$etime[i + 1]
    }

    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start = as.POSIXct(starttimes, tz = "UTC", origin = "1970-01-01"),
                      end = as.POSIXct(endtimes, tz = "UTC", origin = "1970-01-01"),
                      o_slope = as.numeric(oxy_cal_slopes),
                      o_intercept = as.numeric(oxy_cal_ints),
                      o_r2 = as.numeric(oxy_cal_rsq),
                      h_slope = as.numeric(hyd_cal_slopes),
                      h_intercept = as.numeric(hyd_cal_ints),
                      h_r2 = as.numeric(hyd_cal_rsq))

  } else {
    out <- data.frame(start = as.POSIXct(starttimes, tz = "UTC", origin = "1970-01-01"),
                      end = as.POSIXct(endtimes, tz = "UTC", origin = "1970-01-01"),
                      o_slope = as.numeric(NA), o_intercept = as.numeric(NA), o_r2 = as.numeric(NA),
                      h_slope = as.numeric(NA), h_intercept = as.numeric(NA), h_r2 = as.numeric(NA))
  }
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

  print(str(out))

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

  # okay try to write out to h5 file.
  rhdf5::h5createFile(outname)
  rhdf5::h5createGroup(outname, paste0("/", site))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/isoH2o"))

  # okay try to write out to h5 file.
  fid <- rhdf5::H5Fopen(outname)

  # copy attributes from source file and write to output file.
  tmp <- rhdf5::h5readAttributes(inname, paste0("/", site))
  attrloc <- rhdf5::H5Gopen(fid, paste0("/", site))

  for (i in 1:length(tmp)) { # probably a more rapid way to do this in the future...lapply?
    rhdf5::h5writeAttribute(h5obj = attrloc, attr = tmp[[i]], name = names(tmp)[i])
  }

  rhdf5::H5Gclose(attrloc)

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

  #---------------------------------------------
  #---------------------------------------------
  # copy high/mid/low standard data from input file.
  #---------------------------------------------
  #---------------------------------------------
  #low
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))

  low_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))

  low <- rhdf5::h5read(inname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oLow_03m"))

  low <- calibrate_standards_water(out, low)

  # loop through each of the variables in list amb.data.list
  # and write out as a dataframe.
  lapply(names(low), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = low[[x]],
                                     h5loc = low_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})

  rhdf5::H5Gclose(low_outloc)

  #------------------------------------------------------------
  #medium
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))

  med_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))

  med <- rhdf5::h5read(inname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oMed_03m"))

  med <- calibrate_standards_water(out, med)

  # loop through each of the variables in list amb.data.list
  # and write out as a dataframe.
  lapply(names(med), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = med[[x]],
                                     h5loc = med_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})

  rhdf5::H5Gclose(med_outloc)

  #------------------------------------------------------------
  #high
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoH2o/h2oHigh_03m"))

  high_outloc <- rhdf5::H5Gopen(fid,
                                paste0("/", site, "/dp01/data/isoH2o/h2oHigh_03m"))

  high <- rhdf5::h5read(inname,
                        paste0("/", site, "/dp01/data/isoH2o/h2oHigh_03m"))

  high <- calibrate_standards_water(out, high)

  # loop through each of the variables in list amb.data.list
  # and write out as a dataframe.
  lapply(names(high), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = high[[x]],
                                     h5loc = high_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})

  rhdf5::H5Gclose(high_outloc)

  # close the group and the file
  rhdf5::H5Fclose(fid)
  Sys.sleep(0.5)

  rhdf5::h5closeAll()

  fid <- rhdf5::H5Fopen(outname)

  # copy attributes from source file and write to output file.
  tmp <- rhdf5::h5readAttributes(inname, paste0("/", site))
  attrloc <- rhdf5::H5Gopen(fid, paste0("/", site))

  for (i in 1:length(tmp)) { # probably a more rapid way to do this in the future...lapply?
    rhdf5::h5writeAttribute(h5obj = attrloc, attr = tmp[[i]], name = names(tmp)[i])
  }

  rhdf5::H5Gclose(attrloc)

  #===========================================================
  # calibrate data for each height.
  #-------------------------------------
  # extract ambient measurements from ciso
  wiso_logical <- grepl(pattern = "000", x = names(wiso))
  wiso_subset <- wiso[wiso_logical]

  lapply(names(wiso_subset), 
         function(x){calibrate_ambient_water_linreg(amb_data_list = wiso_subset[[x]],
                                                    caldf = out,
                                                    outname = x,
                                                    file = outname,
                                                    site = site,
                                                    r2_thres = r2_thres)})

  rhdf5::h5closeAll()


  print("Copying qfqm...")
  # copy over ucrt and qfqm groups as well.
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/isoH2o"))
  qfqm <- rhdf5::h5read(inname, paste0("/", site, "/dp01/qfqm/isoH2o"))

  lapply(names(qfqm), function(x) {
    copy_qfqm_group(data_list = qfqm[[x]],
                    outname = x, file = outname, site = site, species = "H2O")})

  rhdf5::h5closeAll()

  print("Copying ucrt...")
  # now ucrt.
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/isoH2o"))
  ucrt <- rhdf5::h5read(inname, paste0("/", site, "/dp01/ucrt/isoH2o"))

  lapply(names(ucrt), function(x) {
    copy_ucrt_group(data_list = ucrt[[x]],
                    outname = x, file = outname, site = site, species = "H2O")})

  rhdf5::h5closeAll()

}
