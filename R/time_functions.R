#--------------------------------------------------------------
#' convert_POSIXct_to_NEONhdf5_time
#'
#' Converts a POSIXct object back to the character format used by NEON in their
#' HDF eddy covariance files. Output format,  using strptime syntax,  is
#' %Y-%m-%dT%H:%M:%OSZ.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param intime POSIXct vector to convert to NEON time format.
#'
#' @return Returns character version of POSIXct object
#'         matching NEON time variable format.
#'
#' @export
#' @examples 
#' convert_POSIXct_to_NEONhdf5_time(Sys.time())

convert_POSIXct_to_NEONhdf5_time <- function(intime) {
  
  # convert from POSIXct to
  # a time in YYmmddTHH:MM:SSSZ format used by NEON hdf5 files.
  intime <- lubridate::ymd_hms(intime)
  
  outtime <- as.character(paste0(lubridate::year(intime), "-",
                                 ifelse(lubridate::month(intime) < 10,
                                        paste0("0", lubridate::month(intime)),
                                        lubridate::month(intime)), "-",
                                 ifelse(lubridate::day(intime) < 10,
                                        paste0("0", lubridate::day(intime)),
                                        lubridate::day(intime)), "T",
                                 ifelse(lubridate::hour(intime) < 10,
                                        paste0("0", lubridate::hour(intime)),
                                        lubridate::hour(intime)), ":",
                                 ifelse(lubridate::minute(intime) < 10,
                                        paste0("0", lubridate::minute(intime)),
                                        lubridate::minute(intime)), ":",
                                 ifelse(lubridate::second(intime) < 10,
                                        paste0("0", floor(lubridate::second(intime)), ".000Z"),
                                        paste0(floor(lubridate::second(intime)), ".000Z"))))
  
  return(outtime)
}

#' convert_NEONhdf5_to_POSIXct_time
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param intime Vector of datetimes in NEON data files (as string)
#'           to convert to POSIXct class
#'
#' @return Vector of datetimes from NEON data file now in POSIXct format.
#'
#' @export
#'
#' @examples 
#' convert_NEONhdf5_to_POSIXct_time("2019-06-01T12:00:00.000Z")
convert_NEONhdf5_to_POSIXct_time <- function(intime) {
  
  outtime <- as.POSIXct(intime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", origin = '1970-01-01')
  
  return(outtime)
}
