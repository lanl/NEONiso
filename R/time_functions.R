#--------------------------------------------------------------
#' Convert a POSIXct object to the format used in NEON HDF5 files
#'
#' Converts a POSIXct object back to the character format used by NEON in their
#' HDF eddy covariance files. Output format, using strptime syntax, is
#' %Y-%m-%dT%H:%M:%OSZ. POSIXct input in any time zone is converted to UTC;
#' fractional seconds are truncated.
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

  # POSIXct input is formatted directly in UTC. (It used to go through
  # lubridate::ymd_hms(), which parses as.character(intime): values exactly
  # at midnight lost their time part and became NA, and non-UTC times were
  # relabelled as UTC instead of converted.) Names are dropped, as before.
  # Other input (e.g. character) is still parsed with ymd_hms() first.
  if (!inherits(intime, "POSIXct")) {
    intime <- lubridate::ymd_hms(intime)
  }

  outtime <- format(intime, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC")

  return(unname(outtime))
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

#--------------------------------------------------------------
#' Find which times fall within each calibration period.
#'
#' Returns a list with one element per period: the indices of `times` that
#' fall within [bgn[i], end[i]], i.e. which(times %within% interval(bgn[i],
#' end[i])) for every i.
#'
#' The per-period scan costs O(periods x times). When the periods are finite,
#' non-negative and disjoint under %within%'s own arithmetic (always true for
#' the daily periods from fit_*_regression), each time can only fall in the
#' last period starting at or before it, so findInterval() finds the candidate
#' and %within%'s test is applied to that one period: O(times x log(periods)).
#' Any other input uses the original per-period scan.
#'
#' @param times POSIXct vector of times to assign.
#' @param bgn,end POSIXct vectors of period start and end times.
#' @return list of integer index vectors, one per period.
#' @noRd
calperiod_indices <- function(times, bgn, end) {

  n_per <- length(bgn)

  if (n_per > 0 && length(end) == n_per && inherits(times, "POSIXct") &&
        inherits(bgn, "POSIXct") && inherits(end, "POSIXct")) {
    s <- as.numeric(bgn)
    span <- as.numeric(end) - s
    # %within% tests (t - s) >= 0 & (t - s) <= span. Because (t - s) is
    # nondecreasing in t, diff(s) > span means no time can satisfy that test
    # for period j and also be at or after the start of period j + 1.
    if (all(is.finite(s)) && all(is.finite(span)) && all(span >= 0) &&
          (n_per == 1 || all(diff(s) > span[-n_per]))) {
      a <- as.numeric(times)
      k <- findInterval(a, s)
      cand <- which(!is.na(k) & k > 0)
      d <- a[cand] - s[k[cand]]
      hit <- cand[d <= span[k[cand]] & d >= 0]
      return(unname(split(hit, factor(k[hit], levels = seq_len(n_per)))))
    }
  }

  out <- list()
  for (i in seq_len(n_per)) {
    int <- lubridate::interval(bgn[i], end[i])
    out[[i]] <- which(times %within% int)
  }
  out
}
