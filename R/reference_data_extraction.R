#' extract_carbon_calibration_data.R
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param data_list List containing data, from the /*/dp01/data/
#'                  group in NEON HDF5 file.
#' @param standards Which reference gases (standards) to use? Default is all,
#'        but can pass a subset of "co2Low", "co2Med", and "co2High" as a vector
#'        to this argument as well.
#'
#' @return Returns data frame of required variables.
#'
#' @import tidyselect
#' @import rlang
extract_carbon_cal_data <- function(data_list,
                                    standards = c("co2Low",
                                                  "co2Med",
                                                  "co2High")) {

  # input should be the list from stackEddy
  if (!is.list(data_list)) {
    stop("Input to extract carbon calibration data must be a list")
  }

  # extract desired data from data list.
  data <- data_list %>%
    dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                  tidyselect::starts_with("data.isoCo2.dlta13CCo2"),
                  tidyselect::starts_with("data.isoCo2.rtioMoleDryCo2")) %>%
    dplyr::filter(.data$verticalPosition %in% standards)

  # simplify names
  names(data) <- sub("data.isoCo2.", "", names(data))

  #convert times to posixct
  data$timeBgn <- convert_NEONhdf5_to_POSIXct_time(data$timeBgn)
  data$timeEnd <- convert_NEONhdf5_to_POSIXct_time(data$timeEnd)

  # return standard data frame.
  return(data)
}

#' extract_water_calibration_data
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param data_list List containing data, from the /*/dp01/data/
#'                  group in NEON HDF5 file.
#'
#' @return Returns data frame of required variables.
#'
extract_water_calibration_data <- function(data_list) {

  # input should be the list from stackEddy
  if (!is.list(data_list)) {
    stop("Input to extract_water_calibration_data must be a list")
  }

  # extract desired data from data list.
  data <- data_list %>%
    dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                  tidyselect::starts_with("data.isoH2o.dlta18OH2o"),
                  tidyselect::starts_with("data.isoH2o.dlta2HH2o")) %>%
    dplyr::filter(.data$verticalPosition %in% c("h2oHigh", "h2oMed", "h2oLow"))

  # simplify names
  names(data) <- sub("data.isoH2o.", "", names(data))

  #convert times to posixct
  data$timeBgn <- convert_NEONhdf5_to_POSIXct_time(data$timeBgn)
  data$timeEnd <- convert_NEONhdf5_to_POSIXct_time(data$timeEnd)

  # return standard data frame.
  return(data)
}
