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
#' calibrate_carbon_Bowling2003(inname = fin, outname = "example.h5", 
#'                             site = "ONAQ", write_to_file = FALSE)
calibrate_carbon_bymonth <- function(inname,
                                     outname,
                                     site,
                                     method = "Bowling_2003",
                                     calibration_half_width = 14, # seconds?
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
  #---------------------------------------------------------------
  # Select which validation data to carry through to calibration
  #---------------------------------------------------------------
  refe <- select_daily_reference_data(refe, analyte = 'co2')

  # Okay this function now needs some work. *************
  if (correct_refData == TRUE) {
    
    # do some work to correct the reference data frame
    stds <- correct_carbon_ref_cval(stds,site)
    
  }
  
  if (method == "Bowling_2003") {
    
    # calculate mole fraction (12CO2 / 13CO2) for ref gases and observed values
    refe$conc12CCO2_ref = calculate_12CO2(refe$rtioMoleDryCo2Refe.mean, refe$dlta13CCo2Refe.mean) 
    refe$conc13CCO2_ref = calculate_13CO2(refe$rtioMoleDryCo2Refe.mean, refe$dlta13CCo2Refe.mean)
    refe$conc12CCO2_obs = calculate_12CO2(refe$CO2_obs_mean, refe$dlta13CCo2.mean)
    refe$conc13CCO2_obs = calculate_13CO2(refe$CO2_obs_mean, refe$dlta13CCo2.mean)


  } else if (method == "lin_reg") {
    stop("lin_reg not yet ported over to new structure")
  } else {
    stop("invalid calibration method selected.")
  }
  
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
             r2_thres = r2_thres,
             write_to_file = write_to_file)
         }
  )
  
  rhdf5::h5closeAll()
  
  
  
  #-----------------------------------------------------------
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

  if (write_to_file) {
    setup_output_file(outname, site, 'co2')
    
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
    
    # close the group and the file
    rhdf5::H5Fclose(fid)
    Sys.sleep(0.5)
    
    rhdf5::h5closeAll()
  }
  



  # if (write_to_file) {
  #   
  #   print("Copying qfqm...")
  #   # copy over ucrt and qfqm groups as well.
  #   rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/"))
  #   rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/isoCo2"))
  #   qfqm <- rhdf5::h5read(inname, paste0("/", site, "/dp01/qfqm/isoCo2"))
  #   
  #   lapply(names(qfqm), function(x) {
  #     copy_qfqm_group(data_list = qfqm[[x]],
  #                     outname = x,
  #                     file = outname,
  #                     site = site,
  #                     species = "CO2")})
  #   
  #   rhdf5::h5closeAll()
  #   
  #   print("Copying ucrt...")
  #   # now ucrt.
  #   rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/"))
  #   rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/isoCo2"))
  #   ucrt <- rhdf5::h5read(inname, paste0("/", site, "/dp01/ucrt/isoCo2"))
  #   
  #   lapply(names(ucrt), function(x) {
  #     copy_ucrt_group(data_list = ucrt[[x]],
  #                     outname = x,
  #                     file = outname,
  #                     site = site,
  #                     species = "CO2")})
  #   
  #   rhdf5::h5closeAll()
  #   
  #   Sys.sleep(0.5)
  #   
  # }

}
