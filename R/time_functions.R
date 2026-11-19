#--------------------------------------------------------------
#' Convert a POSIXct object to the format used in NEON HDF5 files
#'
#' Converts a POSIXct object back to the character format used by NEON in their
#' HDF eddy covariance files. Output format, using strptime syntax, is
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

  # Fast path: lubridate::ymd_hms() spends most of its time guessing the
  # format of as.character(intime). For unnamed UTC/GMT POSIXct input with
  # whole-second values between 1970 and 2100, none exactly at midnight, the
  # round trip through ymd_hms() is an identity, so format directly.
  # Every other input (character, other time zones, sub-second or midnight
  # values, which ymd_hms() can turn into NA with a warning) takes the
  # original path below so its behavior is unchanged.
  if (inherits(intime, "POSIXct") && is.null(names(intime)) &&
        isTRUE(attr(intime, "tzone") %in% c("UTC", "GMT"))) {
    secs <- unclass(intime)
    secs <- secs[!is.na(secs)]
    if (length(secs) > 0 &&
          all(secs >= 0 & secs < 4102444800 &
                secs == floor(secs) & secs %% 86400 != 0)) {
      return(format(intime, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC"))
    }
  }

  intime <- lubridate::ymd_hms(intime)

  outtime <- format(intime, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC")

  return(outtime)
}

#' Convert NEON HDF5 file time to POSIXct
#'
#' Converts the date time string in NEON HDF5 files to a POSIXct object
#' for use in R.
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

  outtime <- as.POSIXct(intime,
                        format = "%Y-%m-%dT%H:%M:%OSZ",
                        tz = "GMT",
                        origin = "1970-01-01 00:00:00")

  return(outtime)
}
