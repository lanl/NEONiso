# check_c1_time_conversion.R
# Equivalence check for the convert_POSIXct_to_NEONhdf5_time() fast path (C1).
# Compares the package function against the original implementation on inputs
# that take the fast path and inputs that must fall back, checking both the
# returned value (identical()) and the warnings emitted.
#
# Usage: Rscript benchmarks/check_c1_time_conversion.R [pkg_path]

args <- commandArgs(trailingOnly = TRUE)
pkg_path <- if (length(args)) args[1] else "."
suppressMessages(pkgload::load_all(pkg_path, quiet = TRUE))

original <- function(intime) {
  intime <- lubridate::ymd_hms(intime)
  format(intime, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC")
}

capture <- function(f, x) {
  w <- character()
  v <- withCallingHandlers(tryCatch(f(x), error = function(e) e),
                           warning = function(cnd) {
                             w <<- c(w, conditionMessage(cnd))
                             invokeRestart("muffleWarning")
                           })
  list(value = v, warnings = w)
}

set.seed(20261119)
r <- floor(runif(2e5, 0, 4102444799))
r <- r[r %% 86400 != 0]
days <- floor(runif(2000, 0, 47000)) * 86400

cases <- list(
  random_utc        = .POSIXct(r, tz = "UTC"),
  random_gmt        = .POSIXct(r, tz = "GMT"),
  edges             = .POSIXct(c(days + 1, days + 59, days + 60, days + 3600,
                                 days + 43200, days + 86399), tz = "UTC"),
  with_na           = replace(.POSIXct(r[1:500], tz = "GMT"), c(3, 77), NA),
  length_one        = .POSIXct(r[1], tz = "UTC"),
  # fallback cases:
  midnight_mixed    = .POSIXct(c(r[1:10], days[1:3]), tz = "UTC"),
  midnight_all      = .POSIXct(days[1:5], tz = "UTC"),
  subsecond         = .POSIXct(r[1:100] + 0.0001, tz = "UTC"),
  non_utc           = .POSIXct(r[1:100], tz = "America/Denver"),
  no_tzone          = .POSIXct(r[1:100]),
  named             = setNames(.POSIXct(r[1:3], tz = "UTC"), c("a", "b", "c")),
  all_na            = .POSIXct(rep(NA_real_, 4), tz = "UTC"),
  empty             = .POSIXct(numeric(0), tz = "UTC"),
  pre_1970          = .POSIXct(-r[1:50], tz = "UTC"),
  character_input   = format(.POSIXct(r[1:100], tz = "UTC")),
  cal_df_style      = as.POSIXct(paste(as.Date("2019-05-01") + 0:30,
                                       "00:00:00.0001"), tz = "UTC")
)

ok <- TRUE
for (nm in names(cases)) {
  a <- capture(original, cases[[nm]])
  b <- capture(convert_POSIXct_to_NEONhdf5_time, cases[[nm]])
  same <- identical(a, b)
  ok <- ok && same
  cat(sprintf("%-16s n=%-7d %s\n", nm, length(cases[[nm]]),
              if (same) "IDENTICAL" else "DIFFERS"))
}

x <- .POSIXct(r[1:30000], tz = "GMT")
print(bench::mark(original = original(x),
                  current = convert_POSIXct_to_NEONhdf5_time(x),
                  iterations = 10)[, c("expression", "median", "mem_alloc")])
if (!ok) quit(status = 1)
