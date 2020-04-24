#' extract_carbon_calibration_data.R
#'
#' @param standard String indicating whether to grab data from the high,
#'                  medium, or low standard.
#' @param data.list List containing data, from the /*/dp01/data/
#'                  group in NEON HDF5 file.
#' @param ucrt.list List containing uncertainty data, from the /*/dp01/ucrt/
#'                  group in NEON HDF5 file.
#' @param ucrt.source Where from HDF5 file should  variance be extracted from?
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
    mutate(std_name = standard)

  # return standard data frame.
  return(std_df)

}
