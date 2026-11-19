# check_c4_calibration_windows.R
# Equivalence checks for the per-day subset in fit_*_regression (C4).
#
# Usage:
#   Rscript benchmarks/check_c4_calibration_windows.R subsets [pkg_path]
#     In-process: compares calibration_windows() + numeric test against
#     which(timeBgn %within% lubridate::interval(day - delta, day + delta))
#     for every day, across several half-widths and inputs.
#
#   Rscript benchmarks/check_c4_calibration_windows.R fits <pkg_path> <out.rds>
#     Runs fit_carbon_regression (gainoffset, linreg) and fit_water_regression
#     on the bundled fixture for several half-widths (set.seed(1) before each)
#     and saves the results; run once on the baseline tree and once on the
#     candidate, then compare with identical().

args <- commandArgs(trailingOnly = TRUE)
mode <- args[1]
pkg_path <- if (length(args) >= 2) args[2] else "."
suppressMessages(pkgload::load_all(pkg_path, quiet = TRUE))
library(lubridate, warn.conflicts = FALSE)

fin <- file.path(pkg_path, "inst", "extdata",
                 "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5")
quietly <- function(expr) {
  invisible(capture.output(r <- suppressWarnings(suppressMessages(expr))))
  r
}
half_widths <- c(0.5, 0, 1, 2.75, 14, 100000, -2)

if (mode == "subsets") {
  set.seed(20261119)
  ciso <- quietly(ingest_data(fin, analyte = "Co2", amb_avg = 6, ref_avg = 6))
  t_fix <- extract_carbon_cal_data(ciso$refe_stacked)$timeBgn
  t_rand <- .POSIXct(runif(5000, as.numeric(as.POSIXct("2019-01-01", tz = "UTC")),
                           as.numeric(as.POSIXct("2020-01-01", tz = "UTC"))),
                     tz = "GMT")
  t_na <- t_rand[1:500]; t_na[c(2, 40)] <- NA
  # times exactly at day starts and +/- 12 h, 14 d etc.
  days <- as.POSIXct(as.Date("2019-03-01") + 0:60)
  t_edge <- c(days, days + 43200, days - 43200, days + 14 * 86400,
              days + 2.75 * 86400, days - 1e-6)
  times <- list(fixture = t_fix, random = t_rand, with_na = t_na,
                edges = t_edge)
  date_seq <- as.Date("2019-01-01") + 0:364

  ok <- TRUE
  for (hw in half_widths) {
    delta <- lubridate::ddays(hw)
    w <- calibration_windows(date_seq, delta)
    for (nm in names(times)) {
      tb <- times[[nm]]
      t_bgn <- as.numeric(tb)
      same <- TRUE
      for (i in seq_along(date_seq)) {
        cp <- lubridate::interval(date_seq[i] - delta, date_seq[i] + delta)
        a <- which(tb %within% cp)
        t_off <- t_bgn - w$start[i]
        b <- which(t_off <= w$span[i] & t_off >= 0)
        if (!identical(a, b)) { same <- FALSE; break }
      }
      ok <- ok && same
      cat(sprintf("half_width=%-8s %-8s %s\n", hw, nm,
                  if (same) "IDENTICAL" else paste("DIFFERS at day", i)))
    }
  }
  if (!ok) quit(status = 1)

} else if (mode == "fits") {
  out_file <- args[3]
  ciso <- quietly(ingest_data(fin, analyte = "Co2", amb_avg = 6, ref_avg = 6))
  refe_c <- correct_carbon_ref_cval(
    extract_carbon_cal_data(ciso$refe_stacked), "ONAQ")
  wiso <- quietly(ingest_data(fin, analyte = "H2o", amb_avg = 9, ref_avg = 3))
  refe_w <- swap_standard_isotoperatios(
    extract_water_calibration_data(wiso$refe_stacked))
  res <- list()
  for (hw in half_widths) {
    for (m in c("gainoffset", "linreg")) {
      set.seed(1)
      res[[paste(m, hw)]] <- quietly(tryCatch(
        fit_carbon_regression(refe_c, method = m, calibration_half_width = hw,
                              site = "ONAQ"),
        error = function(e) conditionMessage(e)))
    }
    set.seed(1)
    res[[paste("water", hw)]] <- quietly(tryCatch(
      fit_water_regression(refe_w, calibration_half_width = hw,
                           slope_tolerance = 9999, r2_thres = 0.95,
                           site = "ONAQ"),
      error = function(e) conditionMessage(e)))
  }
  saveRDS(res, out_file)
  cat("saved", length(res), "fits to", out_file, "\n")
}
