#' calibrate_carbon_linreg
#'
#' This function will calibrate NEON carbon isotope data using an ordinary
#' least squares linear regression between measured and reference d13C values.
#' Correction equation is determined by regressing the reference values on
#' the measured values, and uses this equation to calibrate the ambient data.
#' In brief, this function takes the following steps:
#'
#' \enumerate{
#'   \item Extracts calibration data from uncalibrated file.
#'   \item Basic QA/QC on each calibration data point, where the following
#'         factors must be true:
#'   \itemize{
#'     \item Calibration "peak" must have >= 200 data points, to remove some
#'           observed issues with gas manifold valves.
#'     \item Calibration "peak" must not be missing.
#'     \item Only one value per day meeting these criteria are selected.
#'   }
#'   \item Calibration periods are defined to bracket a sample of ambient data.
#'         In many cases, this will be one day bracketed by measurements of
#'        reference materials immediately before after this period.
#'   \item Determine the slope, intercept, and r^2 of a regression of each
#'         calibration period. Calibration error is estimated using
#'         the difference between the model-predicted reference value of each
#'         standard compared to the "known" reference value
#'         for the "medium" standard only. As this bracketing calibration may
#'         use two measurements of the medium standard, only the maximum
#'         difference value is retained as the more conservative approach.
#'
#'   \item Regression parameters are written to a dataset in a new output file
#'         under \code{/site/dp01/data/isoCo2/calData/calRegressions}
#'   \item Regression equations are applied ambient data,
#'         and written to same new output file.
#' }
#'
#' The qfqm and ucrt folders are also copied over from the original file,
#' and are unchanged.
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param site Four-letter NEON code for site being processed. (character)
#' @param inname Name of the input file. (character)
#' @param outname Name of the output file. (character)
#' @param force_cal_to_beginning Extend first calibration to
#'                               the beginning of the file?
#' @param time_diff_between_standards Time (in seconds) required between
#'              consecutive standard measurements.
#'              Used to define a calibration "period."
#' @param force_cal_to_end Extend last calibration to the end of the file?
#' @param r2_thres Minimum r2 threshold of an "acceptable" calibration. Acts to
#'            remove calibration periods where a measurement error makes
#'            relationship nonlinear. Default = 0.95
#' @param correct_refData Should we replace known/suspected incorrect reference
#'            values in the NEON HDF5 files? If \code{TRUE} (default), then
#'            corrects values using a function in \code{standard_corrections.R}.
#'
#' @return nothing to the workspace, but creates a new output file of
#'         calibrated carbon isotope data.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%

calibrate_carbon_linreg <- function(inname,
                                    outname,
                                    site,
                                    time_diff_between_standards = 1800,
                                    force_cal_to_beginning = TRUE,
                                    force_cal_to_end = TRUE,
                                    r2_thres = 0.95,
                                    correct_refData = TRUE) {

  # print status.
  print("Processing carbon calibration data...")
  print("Applying three-point mixing ratio bracketing interpolation")

  f <- 0.00474  # fraction of CO2 isotopomers that aren't 12CO2 or 13CO2
  # note: f technically varies, but this has little impact
  # on calibration per Griffis et al. 2004.

  R_vpdb <- 0.0111797 # 13C/12C ratio for VPD standard.

  ciso <- rhdf5::h5read(inname, paste0("/", site, "/dp01/data/isoCo2"))
  ucrt <- rhdf5::h5read(inname, paste0("/", site, "/dp01/ucrt/isoCo2"))

  high_rs <- extract_carbon_calibration_data(ciso, ucrt, "high")
  med_rs  <- extract_carbon_calibration_data(ciso, ucrt, "med")
  low_rs  <- extract_carbon_calibration_data(ciso, ucrt, "low")

  # cut out period where there appears to be a valve malfunction.
  high_rs <- high_rs %>%
    dplyr::mutate(dom = lubridate::day(d13C_obs_btime)) %>%
    dplyr::group_by(dom) %>%
    dplyr::filter(d13C_obs_n > 200 | is.na(d13C_obs_n)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  med_rs <- med_rs %>%
    dplyr::mutate(dom = lubridate::day(d13C_obs_btime)) %>%
    dplyr::group_by(dom) %>%
    dplyr::filter(d13C_obs_n > 200 | is.na(d13C_obs_n)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  low_rs <- low_rs %>%
    dplyr::mutate(dom = lubridate::day(d13C_obs_btime)) %>%
    dplyr::group_by(dom) %>%
    dplyr::filter(d13C_obs_n > 200 | is.na(d13C_obs_n)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  #=======================================================================
  # apply calibration routines
  #=======================================================================
  # bind together, and cleanup.
  #### OMIT FOR ERROR PROPOAGATION.
  stds <- do.call(rbind, list(low_rs, med_rs, high_rs))
  #stds <- rbind(low_rs, high_rs)
  
  if (correct_refData == TRUE) {
    
    # do some work to correct the reference data frame
    stds <- correct_carbon_ref_cval(stds,site)
    
  }
  
  if (nrow(stds) > 0) {
    # replace NaNs with NA
    # rpf note on 181121 - what does this line actually do? Seems tautological
    # rpf note 181126 - is.na() also returns NaN as NA, so this does actually
    # do what first comment indicates.
    stds[is.na(stds)] <- NA

    # change class of time variables from charatcter to posixct.
    stds$d13C_obs_btime <- as.POSIXct(stds$d13C_obs_btime,
                                      format = "%Y-%m-%dT%H:%M:%OSZ",
                                      tz = "UTC")
    stds$d13C_obs_etime <- as.POSIXct(stds$d13C_obs_etime,
                                      format = "%Y-%m-%dT%H:%M:%OSZ",
                                      tz = "UTC")

    stds$d13C_ref_btime <- as.POSIXct(stds$d13C_ref_btime,
                                      format = "%Y-%m-%dT%H:%M:%OSZ",
                                      tz = "UTC")
    stds$d13C_ref_etime <- as.POSIXct(stds$d13C_ref_etime,
                                      format = "%Y-%m-%dT%H:%M:%OSZ",
                                      tz = "UTC")

    # reorder data frame
    stds <- stds[order(stds$d13C_obs_btime), ]

    # assign a vector corresponding to calibration period.
    stds$cal_period <- stds$d13C_obs_n

    period_id <- 1
    tdiffs <- c(diff(stds$d13C_obs_btime), 0)
    # enforce units of tdiffs to be seconds, otherwise it
    # will occasionally be minutes and produce incorrect output.
    units(tdiffs) <- "secs"

    for (i in 1:nrow(stds)) {
      stds$cal_period[i] <- period_id
      if (tdiffs[i] >= time_diff_between_standards) {
        period_id <- period_id + 1
      }
    }

    # okay, now run calibrations...
    #------------------------------
    # create output variables.
    delta_cal_slopes <- vector()
    delta_cal_ints   <- vector()
    delta_cal_rsq    <- vector()

    co2_cal_slopes <- vector()
    co2_cal_ints   <- vector()
    co2_cal_rsq    <- vector()

    for (i in 2:max(stds$cal_period)) {
      # subset data.
      cal.subset <- stds[which(stds$cal_period == i |
                                 stds$cal_period == (i - 1)), ]

      #---------------------------------------------
      # do some light validation of these points.
      cal.subset <- cal.subset %>%
        dplyr::filter(d13C_obs_var < 5 & abs(CO2_obs_mean - CO2_ref_mean) < 10)

      if (length(unique(cal.subset$std_name)) >= 2 & # at least 2 stds present
          !all(is.na(cal.subset$d13C_obs_mean)) & # not all obs vals missing
          !all(is.na(cal.subset$d13C_ref_mean))) { # not all ref vals missing

        # model to calibrate delta 13C values.
        tmpmod <- lm(d13C_ref_mean ~ d13C_obs_mean, data = cal.subset)

        delta_cal_slopes[i - 1] <- coef(tmpmod)[[2]]
        delta_cal_ints[i - 1] <- coef(tmpmod)[[1]]
        delta_cal_rsq[i - 1] <- summary(tmpmod)$r.squared

        # model to calibrate delta 13C values.
        tmpmod <- lm(CO2_ref_mean ~ CO2_obs_mean, data = cal.subset)

        co2_cal_slopes[i - 1] <- coef(tmpmod)[[2]]
        co2_cal_ints[i - 1] <- coef(tmpmod)[[1]]
        co2_cal_rsq[i - 1] <- summary(tmpmod)$r.squared

      } else {

        delta_cal_slopes[i - 1] <- NA
        delta_cal_ints[i - 1]   <- NA
        delta_cal_rsq[i - 1]    <- NA

        co2_cal_slopes[i - 1] <- NA
        co2_cal_ints[i - 1]   <- NA
        co2_cal_rsq[i - 1]    <- NA

      }
    }

    # make dataframe of calibration data.
    times <- stds %>%
      dplyr::select(d13C_obs_btime, d13C_obs_etime, d13C_ref_btime,
                    d13C_ref_etime, cal_period) %>%
      dplyr::group_by(cal_period) %>%
      dplyr::summarize(etime = max(c(d13C_obs_etime, d13C_ref_etime)))

    # loop through times, assign beginning, ending value.
    # max etime should be just fine.
    starttimes <- vector()
    endtimes <- vector()

    for (i in 1:length(delta_cal_slopes)) {
      starttimes[i] <- times$etime[i]
      endtimes[i] <- times$etime[i + 1]
    }

    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start = as.POSIXct(starttimes,
                                       tz = "UTC",
                                       origin = "1970-01-01"),
                      end = as.POSIXct(endtimes,
                                     tz = "UTC",
                                     origin = "1970-01-01"),
                      d13C_slope = delta_cal_slopes,
                      d13C_intercept = delta_cal_ints,
                      d13C_r2 = delta_cal_rsq,
                      co2_slope = co2_cal_slopes,
                      co2_intercept = co2_cal_ints,
                      co2_r2 = co2_cal_rsq)

  } else {

    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start = as.POSIXct(as.Date("1970-01-01"),
                                       tz = "UTC",
                                       origin = "1970-01-01"),
                      end = as.POSIXct(as.Date("1970-01-01"),
                                       tz = "UTC",
                                       origin = "1970-01-01"),
                      d13C_slope = as.numeric(NA),
                      d13C_intercept = as.numeric(NA),
                      d13C_r2 = as.numeric(NA),
                      co2_slope = as.numeric(NA),
                      co2_intercept = as.numeric(NA),
                      co2_r2 = as.numeric(NA))
  }

  # check to ensure there are 6 columns.
  # add slope, intercept, r2 columns if missing.
  if (!("d13C_slope" %in% names(out))) {
    out$d13C_slope <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("d13C_intercept" %in% names(out))) {
    out$d13C_intercept <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("d13C_r2" %in% names(out))) {
    out$d13C_r2 <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("co2_slope" %in% names(out))) {
    out$co2_slope <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("co2_intercept" %in% names(out))) {
    out$co2_intercept <- as.numeric(rep(NA, length(out$start)))
  }
  if (!("co2_r2" %in% names(out))) {
    out$co2_r2 <- as.numeric(rep(NA, length(out$start)))
  }

  var_for_h5 <- out

  var_for_h5$start <- convert_POSIXct_to_NEONhdf5_time(out$start)
  var_for_h5$end   <- convert_POSIXct_to_NEONhdf5_time(out$end)

  var_for_h5$valid_period_start <- var_for_h5$start
  var_for_h5$valid_period_end   <- var_for_h5$end

  # some columns are stripped when written out to file
  # I think there was a similar issue w/ the Bowling calibration that
  # was solved by enforcing all numeric coolumns to be numeric.
  # *feels super redundant w/ above, but worth a shot i suppose*
  var_for_h5$d13C_slope <- as.numeric(var_for_h5$d13C_slope)
  var_for_h5$co2_slope <- as.numeric(var_for_h5$co2_slope)
  var_for_h5$d13C_intercept <- as.numeric(var_for_h5$d13C_intercept)
  var_for_h5$co2_intercept <- as.numeric(var_for_h5$co2_intercept)
  var_for_h5$d13C_r2 <- as.numeric(var_for_h5$d13C_r2)
  var_for_h5$co2_r2 <- as.numeric(var_for_h5$co2_r2)

  # remove old vars.
  var_for_h5$start <- var_for_h5$end <- NULL

  # okay try to write out to h5 file.
  rhdf5::h5createFile(outname)
  rhdf5::h5createGroup(outname, paste0("/", site))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/isoCo2"))

  fid <- rhdf5::H5Fopen(outname)

  # copy attributes from source file and write to output file.
  tmp <- rhdf5::h5readAttributes(inname, paste0("/", site))

  attrloc <- rhdf5::H5Gopen(fid, paste0("/", site))

  for (i in 1:length(tmp)) { # a more rapid way to do this in future...lapply?
    rhdf5::h5writeAttribute(h5obj = attrloc,
                            attr = tmp[[i]],
                            name = names(tmp)[i])
  }

  rhdf5::H5Gclose(attrloc)

  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/isoCo2/calData"))
  co2_cal_outloc <- rhdf5::H5Gopen(fid,
                                paste0("/", site, "/dp01/data/isoCo2/calData"))

  # write out dataset.
  rhdf5::h5writeDataset.data.frame(obj = var_for_h5,
                                   h5loc = co2_cal_outloc,
                                   name = "calRegressions",
                                   DataFrameAsCompound = TRUE)

  # close the group and the file
  rhdf5::H5Gclose(co2_cal_outloc)

  #---------------------------------------------
  #---------------------------------------------
  # copy high/mid/low standard data from input file.
  #---------------------------------------------
  #---------------------------------------------
  #low
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoCo2/co2Low_09m"))

  low_outloc <- rhdf5::H5Gopen(fid,
                            paste0("/", site, "/dp01/data/isoCo2/co2Low_09m"))

  low <- rhdf5::h5read(inname,
                       paste0("/", site, "/dp01/data/isoCo2/co2Low_09m"))

  low <- calibrate_standards_carbon(out, low, R_vpdb, f,
                                    correct_bad_refvals = TRUE,
                                    site = site, refGas = "low")

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
                       paste0("/", site, "/dp01/data/isoCo2/co2Med_09m"))

  med_outloc <- rhdf5::H5Gopen(fid,
                            paste0("/", site, "/dp01/data/isoCo2/co2Med_09m"))

  med <- rhdf5::h5read(inname,
                       paste0("/", site, "/dp01/data/isoCo2/co2Med_09m"))

  med <- calibrate_standards_carbon(out, med, R_vpdb, f,
                                    correct_bad_refvals = TRUE,
                                    site = site, refGas = "med")

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
                       paste0("/", site, "/dp01/data/isoCo2/co2High_09m"))

  high_outloc <- rhdf5::H5Gopen(fid,
                            paste0("/", site, "/dp01/data/isoCo2/co2High_09m"))

  high <- rhdf5::h5read(inname,
                        paste0("/", site, "/dp01/data/isoCo2/co2High_09m"))

  high <- calibrate_standards_carbon(out, high, R_vpdb, f,
                                     correct_bad_refvals = TRUE,
                                     site = site, refGas = "high")

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

  # calibrate data for each height.
  #-------------------------------------
  # extract ambient measurements from ciso
  ciso_logical <- grepl(pattern = "000", x = names(ciso))
  ciso_subset <- ciso[ciso_logical]

  lapply(names(ciso_subset),
         function(x) {
           calibrate_ambient_carbon_linreg(amb_data_list = ciso_subset[[x]],
                                           caldf = out,
                                           outname = x,
                                           file = outname,
                                           site = site,
                                           r2_thres = r2_thres)

           }) # lapply

  rhdf5::h5closeAll()

  # copy over qfqm and ucrt data groups.
  print("Copying qfqm...")
  # copy over ucrt and qfqm groups as well.
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/isoCo2"))
  qfqm <- rhdf5::h5read(inname, paste0("/", site, "/dp01/qfqm/isoCo2"))

  lapply(names(qfqm), function(x) {
    copy_qfqm_group(data_list = qfqm[[x]],
                    outname = x,
                    file = outname,
                    site = site,
                    species = "CO2")})

  rhdf5::h5closeAll()

  print("Copying ucrt...")
  # now ucrt.
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/isoCo2"))
  ucrt <- rhdf5::h5read(inname, paste0("/", site, "/dp01/ucrt/isoCo2"))

  lapply(names(ucrt), function(x) {
    copy_ucrt_group(data_list = ucrt[[x]],
                    outname = x,
                    file = outname,
                    site = site,
                    species = "CO2")})

  rhdf5::h5closeAll()

}
