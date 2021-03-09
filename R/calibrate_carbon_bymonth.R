#' calibrate_carbon_bymonth
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
#' @param site Four letter NEON site code for site being processed. (character)
#' @param gap_fill_parameters Should function attempt to 'gap-fill' across a 
#'            bad calibration by carrying the last known good calibration forward?
#'            Implementation is fairly primitive currently, as it only carries 
#'            the last known good calibration that's available forward rather
#'            than interpolating, etc. Default FALSE.
#' @param filter_ambient Apply the median absolute deviation filter (Brock 86)
#'            to remove impulse spikes in output ambient data?
#'            (logical; default true)
#' @param r2_thres Minimum r2 threshold of an "acceptable" calibration. Acts to
#'            remove calibration periods where a measurement error makes
#'            relationship nonlinear. Default = 0.95
#' @param correct_refData NEON has indicated there are a few instances where
#'            reported d13C or CO2 reference values are wrong. If set to true,
#'            correct known incorrect values. 
#' @param write_to_file Write calibrated ambient data to file?
#'              (Mostly used for testing)
#'
#' @return Returns nothing to the workspace, but creates a new output HDF5
#'         file containing calibrated carbon isotope values.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%
#' @importFrom stats lm coef
#' 
#' @examples 
#' fin <- system.file("NEON_sample_packed.h5", package = "NEONiso")
#' calibrate_carbon_bymonth(inname = fin, outname = "example.h5", 
#'                             site = "ONAQ", write_to_file = FALSE)
calibrate_carbon_bymonth <- function(inname,
                                     outname,
                                     site,
                                     method = "Bowling_2003",
                                     calibration_half_width = 0.5,
                                     force_cal_to_beginning = TRUE,
                                     force_cal_to_end = TRUE,
                                     gap_fill_parameters = FALSE,
                                     filter_ambient = TRUE,
                                     r2_thres = 0.95,
                                     correct_refData = FALSE, # needs to be fixed.
                                     write_to_file = TRUE) {

  #-----------------------------------------------------------
  # Extract reference data from input HDF5 file.
  #-----------------------------------------------------------
  # pull all carbon isotope data into a list.
  ciso <- neonUtilities::stackEddy(inname, level = 'dp01', avg = 9)
  
  # extract the data we need from ciso list
  refe <- extract_carbon_calibration_data(ciso)
  
  # Okay this function now needs some work. *************
  if (correct_refData == TRUE) {
    
    # do some work to correct the reference data frame
    refe <- correct_carbon_ref_cval(refe,site)
    
  }
  
  # get calibration parameters data.frame.
  cal_df <- fit_carbon_regression(ref_data = refe, method = method,
                                  calibration_half_width = calibration_half_width)

#----------------------------------------------------------------------------
#  calibrate ambient data.
#  extract ambient measurements from ciso
  ciso <- rhdf5::h5read(inname, paste0("/", site, "/dp01/data/isoCo2"))
  ciso_logical <- grepl(pattern = "000", x = names(ciso))
  ciso_subset <- ciso[ciso_logical]

  if (method == "Bowling_2003") {
    ciso_subset_cal <- lapply(names(ciso_subset),
                              function(x) {
                                calibrate_ambient_carbon_Bowling2003(
                                  amb_data_list = ciso_subset[[x]],
                                  caldf = cal_df,
                                  site = site,
                                  # filter_data = filter_ambient,
                                  # force_to_end = force_cal_to_end,
                                  # force_to_beginning = force_cal_to_beginning,
                                  r2_thres = r2_thres)
                              })
  } else if (method == "linreg") {
    ciso_subset_cal <- lapply(names(ciso_subset),
                              function(x) {
                                calibrate_ambient_carbon_linreg(
                                  amb_data_list = ciso_subset[[x]],
                                  caldf = cal_df,
                                  site = site,
                                  # filter_data = filter_ambient,
                                  # force_to_end = force_cal_to_end,
                                  # force_to_beginning = force_cal_to_beginning,
                                  r2_thres = r2_thres,
                                  write_to_file = FALSE)
                              })
  }

  names(ciso_subset_cal) <- names(ciso_subset)
  
  #-----------------------------------------------------------
  # write out these data.frames to a new output file.
  #-----------------------------------------------------------
  if (write_to_file) {
    cal_df$timeBgn <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeBgn)
    cal_df$timeEnd <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeEnd)
    setup_output_file(outname, site, 'co2')
    write_carbon_calibration_data(outname, site, cal_df, method = method)
    write_carbon_ambient_data(outname, site, ciso_subset_cal)
    write_carbon_reference_data(inname, outname, site, cal_df)
    write_qfqm(inname, outname, site, 'co2')
    write_ucrt(inname, outname, site, 'co2')
    
    # one last invokation of hdf5 close all, for good luck
    rhdf5::h5closeAll()
  }

}
