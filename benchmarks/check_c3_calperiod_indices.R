# check_c3_calperiod_indices.R
# Equivalence check for calperiod_indices() (C3) against the original
# per-period loop used in calibrate_ambient_*():
#   for (i in seq_len(nrow(caldf)))
#     l[[i]] <- which(t %within% lubridate::interval(bgn[i], end[i]))
#
# Usage: Rscript benchmarks/check_c3_calperiod_indices.R [pkg_path]

args <- commandArgs(trailingOnly = TRUE)
pkg_path <- if (length(args)) args[1] else "."
suppressMessages(pkgload::load_all(pkg_path, quiet = TRUE))
library(lubridate, warn.conflicts = FALSE)

original <- function(times, bgn, end) {
  out <- list()
  for (i in seq_along(bgn)) {
    int <- lubridate::interval(bgn[i], end[i])
    out[[i]] <- which(times %within% int)
  }
  out
}

daily <- function(d0, n) {
  dates <- as.Date(d0) + 0:(n - 1)
  list(bgn = as.POSIXct(paste(dates, "00:00:00.0001"), tz = "UTC"),
       end = as.POSIXct(paste(dates, "23:59:59.0000"), tz = "UTC"))
}

set.seed(20261119)
p <- daily("2019-01-01", 365)
t_rand <- .POSIXct(sort(runif(50000, as.numeric(p$bgn[1]) - 86400,
                               as.numeric(p$end[365]) + 86400)), tz = "GMT")
# times exactly on / just around every boundary
eps <- c(-1e-4, -1e-6, 0, 1e-6, 1e-4)
t_edge <- .POSIXct(sort(c(outer(as.numeric(p$bgn), eps, "+"),
                          outer(as.numeric(p$end), eps, "+"))), tz = "GMT")
t_na <- t_rand[1:2000]; t_na[c(1, 50, 1999)] <- NA

overl <- p; overl$end[10] <- overl$bgn[12]          # overlapping periods
unsrt <- list(bgn = rev(p$bgn), end = rev(p$end))   # unsorted periods
negsp <- p; negsp$end[5] <- negsp$bgn[5] - 3600      # negative span
nabgn <- p; nabgn$bgn[7] <- NA                       # NA period start
single <- list(bgn = p$bgn[1], end = p$bgn[1])       # zero-length period
touch <- list(bgn = p$bgn, end = c(p$bgn[-1], p$end[365]))  # shared endpoints

# real calibration periods from the bundled fixture
fin <- file.path(pkg_path, "inst", "extdata",
                 "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5")
invisible(capture.output(ciso <- suppressMessages(
  ingest_data(fin, analyte = "Co2", amb_avg = 6, ref_avg = 6))))
refe <- extract_carbon_cal_data(ciso$refe_stacked)
invisible(capture.output(cal <- suppressWarnings(
  fit_carbon_regression(refe, method = "gainoffset",
                        calibration_half_width = 0.5, site = "ONAQ"))))
t_fix <- convert_NEONhdf5_to_POSIXct_time(ciso$ambient[[1]]$dlta13CCo2$timeEnd)

cases <- list(
  random_daily   = list(t_rand, p$bgn, p$end),
  edges_daily    = list(t_edge, p$bgn, p$end),
  with_na_times  = list(t_na, p$bgn, p$end),
  empty_times    = list(t_rand[0], p$bgn, p$end),
  fixture_caldf  = list(t_fix, cal$timeBgn, cal$timeEnd),
  overlapping    = list(t_edge, overl$bgn, overl$end),
  unsorted       = list(t_edge, unsrt$bgn, unsrt$end),
  negative_span  = list(t_edge, negsp$bgn, negsp$end),
  na_period      = list(t_edge, nabgn$bgn, nabgn$end),
  zero_length    = list(.POSIXct(c(as.numeric(p$bgn[1]), 0), tz = "UTC"),
                        single$bgn, single$end),
  shared_ends    = list(t_edge, touch$bgn, touch$end),
  no_periods     = list(t_rand, p$bgn[0], p$end[0])
)

ok <- TRUE
for (nm in names(cases)) {
  x <- cases[[nm]]
  a <- original(x[[1]], x[[2]], x[[3]])
  b <- calperiod_indices(x[[1]], x[[2]], x[[3]])
  same <- identical(a, b)
  ok <- ok && same
  cat(sprintf("%-14s periods=%-4d times=%-6d assigned=%-6d %s\n", nm,
              length(x[[2]]), length(x[[1]]), sum(lengths(a)),
              if (same) "IDENTICAL" else "DIFFERS"))
}

print(bench::mark(original = original(t_rand[1:30000], p$bgn, p$end),
                  current = calperiod_indices(t_rand[1:30000], p$bgn, p$end),
                  iterations = 5)[, c("expression", "median", "mem_alloc")])
if (!ok) quit(status = 1)
