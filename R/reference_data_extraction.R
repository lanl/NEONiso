#' extract_carbon_calibration_data.R
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data_list List containing data, from the /*/dp01/data/
#'                  group in NEON HDF5 file.
#'
#' @return Returns data frame of required variables.
#'
#' @import tidyselect
#' @import rlang
extract_carbon_calibration_data <- function(data_list) {
  
  # input should be the list from stackEddy
  if (!is.list(data_list)) {
    stop("Input to extract carbon calibration data must be a list")
  }
  
  # extract desired data from data list.
  data <- data_list %>% 
    dplyr::select(.data$verticalPosition, .data$timeBgn, .data$timeEnd,
           tidyselect::starts_with("data.isoCo2.dlta13CCo2"),
           tidyselect::starts_with("data.isoCo2.rtioMoleDryCo2")) %>%
    dplyr::filter(.data$verticalPosition %in% c("co2High", "co2Med", "co2Low"))
  
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
#'
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
      
      std_df  <- data.frame(d18O_meas_mean  = data_list$data.isoH2o.dlta18OH2o.mean,
                            d18O_meas_var   = data_list$data.isoH2o.dlta18OH2o.vari,
                            d18O_meas_n     = data_list$data.isoH2o.dlta18OH2o.numSamp,
                            d18O_ref_mean   = data_list$data.isoH2o.dlta18OH2oRefe.mean,
                            d18O_ref_var    = data_list$data.isoH2o.dlta18OH2oRefe.vari,
                            d18O_ref_n      = data_list$data.isoH2o.dlta18OH2oRefe.numSamp,
                            d2H_meas_mean   = data_list$data.isoH2o.dlta2HH2o.mean,
                            d2H_meas_var    = data_list$data.isoH2o.dlta2HH2o.vari,
                            d2H_meas_n      = data_list$data.isoH2o.dlta2HH2o.numSamp,
                            d2H_ref_mean    = data_list$data.isoH2o.dlta2HH2oRefe.mean,
                            d2H_ref_var     = data_list$data.isoH2o.dlta2HH2oRefe.vari,
                            d2H_ref_n       = data_list$data.isoH2o.dlta2HH2oRefe.numSamp,
                            btime           = data_list$timeBgn,
                            etime           = data_list$timeEnd)
      
      #convert times to posixct
      std_df$btime <- as.POSIXct(std_df$btime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
      std_df$etime <- as.POSIXct(std_df$etime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
      
      #kludge fix here - need to fix time variables in a future release!!
      std_df$d18O_meas_btime <- std_df$btime

    } # ucrt source
    
  } else if (method == "by_month") {
    
    if (standard == "high") {
      data <- data_list
    } else if (standard == "med") {
      data <- data_list
    } else if (standard == "low") {
      data <- data_list
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
      
      # change class of time variables from charatcter to posixct.
      std_df$d18O_meas_btime <- convert_NEONhdf5_to_POSIXct_time(std_df$d18O_meas_btime)
      std_df$d18O_meas_etime <- convert_NEONhdf5_to_POSIXct_time(std_df$d18O_meas_etime)

      std_df$d18O_ref_btime <- convert_NEONhdf5_to_POSIXct_time(std_df$d18O_ref_btime)
      std_df$d18O_ref_etime <- convert_NEONhdf5_to_POSIXct_time(std_df$d18O_ref_etime)

      std_df$d2H_meas_btime <- convert_NEONhdf5_to_POSIXct_time(std_df$d2H_meas_btime)
      std_df$d2H_meas_etime <- convert_NEONhdf5_to_POSIXct_time(std_df$d2H_meas_etime)

      std_df$d2H_ref_btime <- convert_NEONhdf5_to_POSIXct_time(std_df$d2H_ref_btime)
      std_df$d2H_ref_etime <- convert_NEONhdf5_to_POSIXct_time(std_df$d2H_ref_etime)
      
    }
  } else {
    stop("no other methods have been coded")
  }
  
  # add standard name
  std_df <- std_df %>%
    dplyr::mutate(std_name = standard)
  
  return(std_df)
  
}