# filtering_functions.R
# a file with a few option for filtering output data.
#---------------------------------
#' Apply a median absolute deviation filter
#'
#' Median absolute deviation filter of Brock 1986, with user specified
#' width and magnitude thresholds.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param data Vector to filter.
#' @param width Width of filter, in rows.
#' @param threshold Only filter values that are `abs(threshold)`
#'              away from median
#'
#' @return Returns filtered vector.
#'
#' @importFrom stats median
filter_median_brock86 <- function(data, width = 7, threshold = 5) {

  # get rolling median of data.
  filt <- rolling_median(data, width)

  # get logical vector of what indices are valid.
  spikes <- abs(data - filt) > threshold

  # set spikes as missing - consider interpolation in future release?
  data[spikes == TRUE] <- NA

  # return data vector back
  return(data)

}

#' Centered rolling median, ignoring NAs
#'
#' Same result as
#' `zoo::rollapply(x, width, median, na.rm = TRUE, fill = NA)`, which calls
#' `median()` once per window (~10 us per element). For a plain double
#' vector and odd `width`, this instead sorts every window at once with a
#' single `order()` call (window id, then value, NAs last) and reads each
#' median from its window's sorted values: the middle value when the window
#' has an odd number of non-NA values, else `mean()` of the two middle values,
#' exactly as `median()` does. Other inputs use `zoo::rollapply()`.
#'
#' @param x Vector to smooth.
#' @param width Window width, in rows.
#'
#' @noRd
rolling_median <- function(x, width) {

  n <- length(x)
  if (!is.double(x) || !is.null(attributes(x)) || length(width) != 1 ||
        !isTRUE(width >= 1 && width %% 2 == 1) || n < width) {
    return(zoo::rollapply(x, width, median, na.rm = TRUE, fill = NA))
  }

  half <- (width - 1) %/% 2
  centers <- (half + 1):(n - half)
  n_win <- length(centers)

  # one row per window, sorted ascending with NAs (and NaNs) last
  vals <- x[outer(centers, -half:half, "+")]
  win <- rep(seq_len(n_win), times = width)
  sorted <- matrix(vals[order(win, vals, na.last = TRUE)],
                   nrow = n_win, byrow = TRUE)
  n_ok <- rowSums(!is.na(sorted))

  med <- rep(NA_real_, n_win)
  odd <- which(n_ok %% 2 == 1)
  med[odd] <- sorted[cbind(odd, (n_ok[odd] + 1) / 2)]
  even <- which(n_ok > 0 & n_ok %% 2 == 0)
  lo <- sorted[cbind(even, n_ok[even] / 2)]
  hi <- sorted[cbind(even, n_ok[even] / 2 + 1)]
  med[even] <- vapply(seq_along(even), function(j) mean(c(lo[j], hi[j])),
                      numeric(1))

  out <- rep(NA_real_, n)
  out[centers] <- med
  out
}
