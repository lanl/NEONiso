#' extract_carbon_calibration_data.R
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param standard String indicating whether to grab data from the high,
#'                  medium, or low standard.
#' @param data_list List containing data, from the /*/dp01/data/
#'                  group in NEON HDF5 file.
#' @param ucrt_list List containing uncertainty data, from the /*/dp01/ucrt/
#'                  group in NEON HDF5 file.
#' @param ucrt_source Where from HDF5 file should  variance be extracted from?
#'                    (Valid values are "data" and "ucrt")
#'
#' @return Returns data frame of required variables.
#' @export
#'
#' @examples
extract_carbon_calibration_data <- function(data_list, ucrt_list,
                                            standard, ucrt_source = "data") {

  if (standard == "high") {
    data <- data_list$co2High_09m
    ucrt <- ucrt_list$co2High_09m
  } else if (standard == "med") {
    data <- data_list$co2Med_09m
    ucrt <- ucrt_list$co2Med_09m
  } else if (standard == "low") {
    data <- data_list$co2Low_09m
    ucrt <- ucrt_list$co2Low_09m
  } else {
    stop()
  }

  if (ucrt_source == "ucrt") {

    std_df <- data.frame(d13C_obs_mean = data$dlta13CCo2$mean,
                         d13C_obs_n = data$dlta13CCo2$numSamp,
                         d13C_obs_btime = data$dlta13CCo2$timeBgn,
                         d13C_obs_etime = data$dlta13CCo2$timeEnd,
                         d13C_ref_mean = data$dlta13CCo2Refe$mean,
                         d13C_ref_btime = data$dlta13CCo2Refe$timeBgn,
                         d13C_ref_etime = data$dlta13CCo2Refe$timeEnd,
                         d13C_obs_var = ucrt$dlta13CCo2$vari,
                         d13C_ref_var = data$dlta13CCo2Refe$vari,
                         CO2_obs_mean = data$rtioMoleDryCo2$mean,
                         CO2_obs_n = data$rtioMoleDryCo2$numSamp,
                         CO2_ref_mean = data$rtioMoleDryCo2Refe$mean,
                         CO2_ref_var = data$rtioMoleDryCo2Refe$vari,
                         CO2_obs_var = ucrt$rtioMoleDryCo2$vari)

  } else if (ucrt_source == "data") {

    std_df <- data.frame(d13C_obs_mean = data$dlta13CCo2$mean,
                         d13C_obs_n = data$dlta13CCo2$numSamp,
                         d13C_obs_btime = data$dlta13CCo2$timeBgn,
                         d13C_obs_etime = data$dlta13CCo2$timeEnd,
                         d13C_ref_mean = data$dlta13CCo2Refe$mean,
                         d13C_ref_btime = data$dlta13CCo2Refe$timeBgn,
                         d13C_ref_etime = data$dlta13CCo2Refe$timeEnd,
                         d13C_obs_var = data$dlta13CCo2$vari,
                         d13C_ref_var = data$dlta13CCo2Refe$vari,
                         CO2_obs_mean = data$rtioMoleDryCo2$mean,
                         CO2_obs_n = data$rtioMoleDryCo2$numSamp,
                         CO2_ref_mean = data$rtioMoleDryCo2Refe$mean,
                         CO2_ref_var = data$rtioMoleDryCo2Refe$vari,
                         CO2_obs_var = data$rtioMoleDryCo2$vari)

  }

  # add standard name
  std_df <- std_df %>%
    dplyr::mutate(std_name = standard)

  # return standard data frame.
  return(std_df)

}


#' extract_water_calibration_data
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param standard String indicating whether to grab data from the high,
#'                  medium, or low standard.
#' @param data_list List containing data, from the /*/dp01/data/
#'                  group in NEON HDF5 file.
#' @param ucrt_list List containing uncertainty data, from the /*/dp01/ucrt/
#'                  group in NEON HDF5 file. (only works if paired with ucrt_source = 'ucrt'
#'                  and method = 'by_month')
#' @param ucrt_source Where from HDF5 file should  variance be extracted from?
#'                    (Only "data" works now..."ucrt" will throw an error.)
#' @param method Are we calling this function from the calibrate_water_linreg
#'               function (use "by_month") or the calibrate_water_linreg_bysite
#'               function (use "by_site)
#'
#' @return Returns data frame of required variables.
#' @export
#'
#' @examples
extract_water_calibration_data <- function(data_list, ucrt_list = NULL,
                                           standard, ucrt_source = "data",
                                           method = "by_site") {
  
  if (method == "by_site") {
    
    if (ucrt_source == "ucrt") {
      
      stop("ucrt data handling not added yet...please change ucrt_source to 'data'")
      
    } else if (ucrt_source == "data") {
    
      if (standard == "high") {
        data <- subset(data_list, data_list$verticalPosition == 'h2oHigh')
      } else if (standard == "med") {
        data <- subset(data_list, data_list$verticalPosition == 'h2oMed')
      } else if (standard == "low") {
        data <- subset(data_list, data_list$verticalPosition == 'h2oLow')
      } else {
        stop()
      }
        
      std_df  <- data.frame(d18O_meas_mean  = data$data.isoH2o.dlta18OH2o.mean,
                            d18O_meas_var   = data$data.isoH2o.dlta18OH2o.vari,
                            d18O_meas_n     = data$data.isoH2o.dlta18OH2o.numSamp,
                            d18O_ref_mean   = data$data.isoH2o.dlta18OH2oRefe.mean,
                            d18O_ref_var    = data$data.isoH2o.dlta18OH2oRefe.vari,
                            d18O_ref_n      = data$data.isoH2o.dlta18OH2oRefe.numSamp,
                            d2H_meas_mean   = data$data.isoH2o.dlta2HH2o.mean,
                            d2H_meas_var    = data$data.isoH2o.dlta2HH2o.vari,
                            d2H_meas_n      = data$data.isoH2o.dlta2HH2o.numSamp,
                            d2H_ref_mean    = data$data.isoH2o.dlta2HH2oRefe.mean,
                            d2H_ref_var     = data$data.isoH2o.dlta2HH2oRefe.vari,
                            d2H_ref_n       = data$data.isoH2o.dlta2HH2oRefe.numSamp,
                            btime           = data$timeBgn,
                            etime           = data$timeEnd)
    }
  } else if (method == "by_month") {
    
    if (standard == "high") {
      data <- data_list$co2High_09m
      ucrt <- ucrt_list$co2High_09m
    } else if (standard == "med") {
      data <- data_list$co2Med_09m
      ucrt <- ucrt_list$co2Med_09m
    } else if (standard == "low") {
      data <- data_list$co2Low_09m
      ucrt <- ucrt_list$co2Low_09m
    } else {
      stop()
    }
    
    if (ucrt_source == "ucrt") {
      
      stop("ucrt data handling not added yet...please change ucrt_source to 'data'")
      
    } else if (ucrt_source == "data") {
      
      std_df <- data.frame(d18O_meas_mean = data$dlta18OH2o$mean,
                           d18O_meas_var = data$dlta18OH2o$vari,
                           d18O_meas_n = data$dlta18OH2o$numSamp,
                           d18O_meas_btime = data$dlta18OH2o$timeBgn,
                           d18O_meas_etime = data$dlta18OH2o$timeEnd,
                           d18O_ref_mean = data$dlta18OH2oRefe$mean,
                           d18O_ref_var = data$dlta18OH2oRefe$vari,
                           d18O_ref_n = data$dlta18OH2oRefe$numSamp,
                           d18O_ref_btime = data$dlta18OH2oRefe$timeBgn,
                           d18O_ref_etime = data$dlta18OH2oRefe$timeEnd,
                           d2H_meas_mean = data$dlta2HH2o$mean,
                           d2H_meas_var = data$dlta2HH2o$vari,
                           d2H_meas_n = data$dlta2HH2o$numSamp,
                           d2H_meas_btime = data$dlta2HH2o$timeBgn,
                           d2H_meas_etime = data$dlta2HH2o$timeEnd,
                           d2H_ref_mean = data$dlta2HH2oRefe$mean,
                           d2H_ref_var = data$dlta2HH2oRefe$vari,
                           d2H_ref_n = data$dlta2HH2oRefe$numSamp,
                           d2H_ref_btime = data$dlta2HH2oRefe$timeBgn,
                           d2H_ref_etime = data$dlta2HH2oRefe$timeEnd)
    }
  } else {
    stop("no other methods have been coded")
  }
  
  return(std_df)
  
}