#' calibrate_carbon_Bowling2003
#'
#' Use the gain-and-offset style calibration approach detailed in
#' Bowling et al. 2003 AFM. Wen et al. 2013 compared several different carbon
#' isotope calibration techniques and found this to be the superior method
#' under most circumstances. In brief, this method estimates gain and offset
#' parameters using linear regression on 12CO2 and 13CO2 isotopologues
#' separately. These gain and offset parameters are analogous to regression
#' slope and intercepts, but jointly correct for CO2 concentration dependence
#' and place d13C values on the VPDB scale. Gain and offset parameters are 
#' determined independently for each major isotopologue (12CO2 and 13CO2).
#' For the two reference materials
#' selected, the gain and offset parameters are defined by:
#'
#' \deqn{G = (X_{2,ref}-X_{1,ref})/(X_{2,meas}-X_{1,meas})}
#' \deqn{O = X_{2,ref}- G X_{2,meas}}
#' Calibrated ambient isotopologues are then given as:
#' \deqn{X_{cal} = X_{meas} G + O}
#'
#' Measurements of reference materials were considered "good" if the following
#' conditions were met:
#' \itemize{
#'   \item Measured CO2 concentrations were within 10 ppm
#'         of known "reference" concentrations.
#'   \item Variance of the CO2 concentration in standard peak was < 5 ppm.
#'   \item Measured d13C value must be within 5 per mil
#'         of known "reference" d13C value.
#' }
#' The first two criteria are intended to filter out periods where there is
#' a clear issue with the gas delivery system (i.e., nearly empty gas tank,
#' problem with a valve in the manifold, etc.); the third criterion was adopted
#' after visual inspection of data timeseries revealed that often the first
#' standard measurement following an instrument issue had higher-than-expected
#' error. This criterion clips clearly poor values.
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param inname Name of the input file. (character)
#' @param outname Name of the output file. (character)
#' @param time_diff_btwn_cals Time (in seconds) required between
#'             consecutive standard measurements. (numeric)
#' @param force_cal_to_beginning Extend first calibration to the beginning
#'             of the file? (CURRENTLY NOT USED)
#' @param force_cal_to_end Extend last calibration to the end of the file?
#'             (CURRENTLY NOT USED)
#' @param ucrt.source Where do we take uncertainty estimates from
#'            (data variance, or the ucrt group in the hdf5 file?)
#' @param site Four letter NEON site code for site being processed. (character)
#' @param interpolate.missing.cals If a calibration is flagged as bad, should
#'             we replace these calibrations by interpolating from the closest
#'             good calibrations? (logical)
#' @param interpolation.method How to interpolate across bad calibrations,
#'            if \code{interpolate.missing.cals == TRUE}. (character)
#' @param filter_ambient Apply the median absolute deviation filter (Brock 86)
#'            to remove impulse spikes in output ambient data?
#'            (logical; default true)
#' @param r2_thres Minimum r2 threshold of an "acceptable" calibration. Acts to
#'            remove calibration periods where a measurement error makes
#'            relationship nonlinear. Default = 0.95
#' @param correct_refData NEON has indicated there are a few instances where
#'            reported d13C or CO2 reference values are wrong. If set to true,
#'            correct known incorrect values. 
#'
#' @return Returns nothing to the workspace, but creates a new output HDF5
#'         file containing calibrated carbon isotope values.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%
#' @importFrom stats lm coef
calibrate_carbon_Bowling2003 <- function(inname,
                                         outname,
                                         site,
                                         time_diff_btwn_cals = 1800,
                                         force_cal_to_beginning = TRUE,
                                         force_cal_to_end = TRUE,
                                         interpolate.missing.cals = FALSE,
                                         interpolation.method = "LWMA",
                                         ucrt.source = "data",
                                         filter_ambient = TRUE,
                                         r2_thres = 0.95,
                                         correct_refData = TRUE) {

  #------------------------------------------------------------
  # Print some information before starting data processing
  #------------------------------------------------------------
  print("Processing carbon calibration data...")
  print("Applying method 1: linear regression on 12CO2 and 13CO2 separately.")
  print("Reference: Bowling et al. 2003 AFM")
  #-----------------------------------------------------------
  # pull all carbon isotope data into a list.

  ciso <- rhdf5::h5read(inname, paste0("/", site, "/dp01/data/isoCo2"))
  ucrt <- rhdf5::h5read(inname, paste0("/", site, "/dp01/ucrt/isoCo2"))

  high_rs <- extract_carbon_calibration_data(ciso, ucrt, "high")
  med_rs  <- extract_carbon_calibration_data(ciso, ucrt, "med")
  low_rs  <- extract_carbon_calibration_data(ciso, ucrt, "low")
  
  #---------------------------------------------------------------
  # Select which validation data to carry through to calibration
  #---------------------------------------------------------------
  high_rs <- select_daily_reference_data(high_rs, analyte = 'co2')
  med_rs  <- select_daily_reference_data(med_rs, analyte = 'co2')
  low_rs  <- select_daily_reference_data(low_rs, analyte = 'co2')
  
  # merge standards back to a single df.
  stds <- do.call(rbind, list(low_rs, med_rs, high_rs))
  #stds <- rbind(high_rs, med_rs)
  
  if (correct_refData == TRUE) {
    
    # do some work to correct the reference data frame
    stds <- correct_carbon_ref_cval(stds,site)
    
  }
  
  #------------------------------------------------------------
  # calculate mole fraction (12CO2 / 13CO2) for ref gases and observed values
  stds$conc12CCO2_ref = calculate_12CO2(stds$CO2_ref_mean, stds$d13C_ref_mean) 
  stds$conc13CCO2_ref = calculate_13CO2(stds$CO2_ref_mean, stds$d13C_ref_mean)
  stds$conc12CCO2_obs = calculate_12CO2(stds$CO2_obs_mean, stds$d13C_obs_mean)
  stds$conc13CCO2_obs = calculate_13CO2(stds$CO2_obs_mean, stds$d13C_obs_mean)
  
  # reorder to be in chronological time.
  stds <- stds[order(stds$d13C_obs_btime), ]

  if (nrow(stds) > 0) {

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

  } else {

    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start = as.POSIXct(as.Date("1970-01-01"),
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
  }

  var_for_h5 <- out

  var_for_h5$start <- convert_POSIXct_to_NEONhdf5_time(out$start)
  var_for_h5$end <- convert_POSIXct_to_NEONhdf5_time(out$end)

  var_for_h5$valid_period_start <- var_for_h5$start
  var_for_h5$valid_period_end   <- var_for_h5$end

  # enforce that all other columns are numeric
  var_for_h5$gain12C   <- as.numeric(var_for_h5$gain12C)
  var_for_h5$gain13C   <- as.numeric(var_for_h5$gain13C)
  var_for_h5$offset12C <- as.numeric(var_for_h5$offset12C)
  var_for_h5$offset13C <- as.numeric(var_for_h5$offset13C)
  var_for_h5$r2_12C    <- as.numeric(var_for_h5$r2_12C)
  var_for_h5$r2_13C    <- as.numeric(var_for_h5$r2_13C)

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

  for (i in 1:length(tmp)) {
    # probably a more rapid way to do this in the future...lapply?
    rhdf5::h5writeAttribute(h5obj = attrloc,
                            attr = tmp[[i]],
                            name = names(tmp)[i])
  }

  rhdf5::H5Gclose(attrloc)

  # write out calibration dataframe to a new group to keep it away from stackEddy
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoCo2/calData"))

  co2.cal.outloc <- rhdf5::H5Gopen(fid,
                                paste0("/", site, "/dp01/data/isoCo2/calData"))

  # write out dataset.
  rhdf5::h5writeDataset.data.frame(obj = var_for_h5,
                                   h5loc = co2.cal.outloc,
                                   name = "calGainsOffsets",
                                   DataFrameAsCompound = TRUE)

  rhdf5::H5Gclose(co2.cal.outloc)

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

#  low <- calibrate_standards_carbon(out, low, R_vpdb, f)
  low <- calibrate_standards_carbon(out, low, correct_bad_refvals = TRUE,
                                    site = site, refGas = "low")
  
  # loop through each variable amb.data.list and write out as a dataframe.
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

#  med <- calibrate_standards_carbon(out, med, R_vpdb, f)
  med <- calibrate_standards_carbon(out, med, correct_bad_refvals = TRUE,
                                    site = site, refGas = "med")

  # loop through each variable in amb.data.list and write out as a dataframe.
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

  #high <- calibrate_standards_carbon(out, high, R_vpdb, f)
  high <- calibrate_standards_carbon(out, high, correct_bad_refvals = TRUE,
                                     site = site, refGas = "high")
  
  # loop through each variable amb.data.list and write out as a dataframe.
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
  #----------------------------------------------------------------------------
  # calibrate ambient data.
  # extract ambient measurements from ciso
  ciso_logical <- grepl(pattern = "000", x = names(ciso))
  ciso_subset <- ciso[ciso_logical]

  lapply(names(ciso_subset),
         function(x) {
           calibrate_ambient_carbon_Bowling2003(
             amb_data_list = ciso_subset[[x]],
             caldf = out,
             outname = x,
             file = outname,
             site = site,
             filter_data = filter_ambient,
             force_to_end = force_cal_to_end,
             force_to_beginning = force_cal_to_beginning,
             r2_thres = r2_thres)
         }
  )

  rhdf5::h5closeAll()

  # # copy irga data??
  # cirga <- rhdf5::h5read(inname, paste0("/", site, "/dp01/data/co2Stor/"))
  # 
  # #copy over irga data
  # rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/co2Stor/"))
  # 
  # copy_irga_groups <- function(data_list, outname, site, file) {
  #   fid <- rhdf5::H5Fopen(file)
  #   
  #   data_outloc <- rhdf5::H5Gcreate(fid,
  #                                   paste0("/", site, "/dp01/data/co2Stor/", outname))
  #   
  #   # loop through variables and copy to out.
  #   lapply(names(data_list), function(x) {
  #     rhdf5::h5writeDataset.data.frame(obj = data_list[[x]],
  #                                      h5loc = data_outloc,
  #                                      name = x,
  #                                      DataFrameAsCompound = TRUE)
  #   })
  # }

  # lapply(names(cirga), function(x) {
  #   copy_irga_groups(data_list = cirga[[x]],
  #                   outname = x,
  #                   file = outname,
  #                   site = site)})
  # 
  # rhdf5::h5closeAll()
  
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

  Sys.sleep(0.5)

}
