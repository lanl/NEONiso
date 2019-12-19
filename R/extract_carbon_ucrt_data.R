#' Title
#'
#' @param isoCo2.list 
#' @param standard 
#'
#' @return
#' @export
#'
#' @examples
extract_carbon_calibration_data <- function(ucrt.list,standard) {
  
  require(dplyr)
  
  if (standard == "high") {
    std <- ucrt.list$co2High_09m
  } else if (standard == "med") {
    std <- ucrt.list$co2Med_09m
  } else if (standard == "low") {
    std <- ucrt.list$co2Low_09m
  } else {
    stop()
  }
  
  std.df <- data.frame(d13C_obs_mean=std$dlta13CCo2$mean,
                        d13C_obs_var=std$dlta13CCo2$vari,
                        d13C_obs_n=std$dlta13CCo2$numSamp,
                        d13C_obs_btime=std$dlta13CCo2$timeBgn,
                        d13C_obs_etime=std$dlta13CCo2$timeEnd,
                        CO2_obs_mean=std$rtioMoleDryCo2$mean,
                        CO2_obs_var=std$rtioMoleDryCo2$vari,
                        CO2_obs_n=std$rtioMoleDryCo2$numSamp,
                        d13C_ref_mean=std$dlta13CCo2Refe$mean,
                        d13C_ref_var=std$dlta13CCo2Refe$vari,
                        d13C_ref_n=std$dlta13CCo2Refe$numSamp,
                        d13C_ref_btime=std$dlta13CCo2Refe$timeBgn,
                        d13C_ref_etime=std$dlta13CCo2Refe$timeEnd,
                        CO2_ref_mean=std$rtioMoleDryCo2Refe$mean,
                        CO2_ref_var=std$rtioMoleDryCo2Refe$vari)
  
  # add standard name
  std.df <- std.df %>%
    mutate(std_name=standard)
  
  # return standard data frame.
  return(std.df)
  
}