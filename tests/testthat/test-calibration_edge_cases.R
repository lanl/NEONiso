# Regression tests for calibration edge cases that were previously handled
# incorrectly. Each test states the old behavior and what is expected now,
# so a failure here means one of these bugs has come back.

fin <- system.file("extdata",
                   "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
                   package = "NEONiso", mustWork = TRUE)

# ---- Time conversion -------------------------------------------------------

# convert_POSIXct_to_NEONhdf5_time() used to round-trip POSIXct input through
# lubridate::ymd_hms(as.character(x)). as.character() drops the time part for
# values exactly at midnight, so those became NA with a parse warning (this is
# how calibration-period start times at 00:00:00 could be written as NA, see
# #66). Non-UTC input was also relabelled as UTC instead of being converted.
test_that("midnight timestamps convert to strings, not NA", {
  midnight <- as.POSIXct("2019-05-01 00:00:00", tz = "UTC")
  expect_no_warning(out <- convert_POSIXct_to_NEONhdf5_time(midnight))
  expect_identical(out, "2019-05-01T00:00:00.000Z")

  # vectors that are all at midnight used to fail entirely
  days <- as.POSIXct(c("2019-05-01", "2019-05-02"), tz = "UTC")
  expect_identical(convert_POSIXct_to_NEONhdf5_time(days),
                   c("2019-05-01T00:00:00.000Z", "2019-05-02T00:00:00.000Z"))
})

test_that("non-UTC times are converted to UTC, not relabelled", {
  denver <- as.POSIXct("2019-05-01 03:00:00", tz = "America/Denver") # UTC-6
  expect_identical(convert_POSIXct_to_NEONhdf5_time(denver),
                   "2019-05-01T09:00:00.000Z")
})

test_that("character input is still parsed as UTC", {
  expect_identical(convert_POSIXct_to_NEONhdf5_time("2019-05-01 03:00:00"),
                   "2019-05-01T03:00:00.000Z")
})

# ---- Isotopologue conversions -------------------------------------------

# calculate_13CO2() computes 13CO2 as total * (1 - f) - 12CO2, but used to
# compute the 12CO2 term with the default f, so a non-default f was applied
# to one term and not the other. With f passed through consistently, the two
# isotopologues sum to total * (1 - f) for any f.
test_that("calculate_13CO2 applies a non-default f to both terms", {
  for (f in c(0.00474, 0.01)) {
    total <- calculate_12CO2(400, -8, f = f) + calculate_13CO2(400, -8, f = f)
    expect_equal(total, 400 * (1 - f))
  }
})

# ---- Daily calibration windows ----------------------------------------------

# fit_carbon_regression() builds one calibration window per day. Windows used
# to be [00:00:00.0001, 23:59:59], so an ambient observation ending in the last
# second of a day matched no window and was left uncalibrated. Windows must
# now start at midnight, not overlap, and leave no gap wider than 1 ms (the
# resolution of NEON timestamps).
test_that("daily calibration windows are contiguous", {
  co2 <- ingest_data(fin, analyte = "Co2", amb_avg = 9, ref_avg = 9)
  refe <- NEONiso:::extract_carbon_cal_data(co2$refe_stacked)
  for (m in c("gainoffset", "linreg")) {
    cal <- NEONiso:::fit_carbon_regression(refe, method = m,
                                           calibration_half_width = 0.5,
                                           site = "ONAQ")
    n <- nrow(cal)
    expect_gt(n, 1)
    bgn <- as.numeric(cal$timeBgn)
    end <- as.numeric(cal$timeEnd)
    gap <- bgn[-1] - end[-n]
    expect_equal(bgn %% 86400, rep(0, n))   # each window starts at midnight
    expect_true(all(gap > 0))              # consecutive windows do not overlap
    expect_true(all(gap <= 0.0011))        # and leave at most ~1 ms between
  }
})

# ---- Extending calibrations to the ends of the record --------------------

# force_cal_to_beginning / force_cal_to_end are meant to extend the first and
# last calibration periods to cover all ambient data. They used to assign to
# columns that do not exist (start/end instead of timeBgn/timeEnd), so they
# had no effect. The fixture's last carbon ambient observation ends after the
# last calibration day, so it is only calibrated when force_cal_to_end works.
test_that("force_cal_to_end calibrates carbon data after the last cal day", {
  run <- function(force) {
    suppressWarnings(calibrate_carbon(fin, tempfile(), "ONAQ",
      method = "gainoffset", calibration_half_width = 0.5,
      force_cal_to_end = force, force_cal_to_beginning = force,
      write_to_file = FALSE))$ciso_subset_cal$`000_010_09m`$dlta13CCo2
  }
  on <- run(TRUE)
  off <- run(FALSE)
  last <- nrow(on)
  expect_true(is.na(off$mean_cal[last]))
  expect_false(is.na(on$mean_cal[last]))
  # every other observation is unaffected
  expect_identical(on$mean_cal[-last], off$mean_cal[-last])
})

# ---- Ambient data outside every calibration period -------------------------

# Water (and carbon linreg) calibration used to start the calibrated columns as
# copies of the raw values and only overwrite rows inside a calibration
# period, so rows outside every period were reported as calibrated while
# holding raw values. They must now be NA unless the calibration is
# explicitly extended to cover them (see #105 for better handling). In the
# fixture, water calibration periods start on May 7, while ambient data start
# on May 1.
test_that("water data before the first calibration period are NA, not raw", {
  run <- function(force) {
    calibrate_water(fin, tempfile(), "ONAQ", write_to_file = FALSE,
                    force_cal_to_beginning = force)
  }
  h2o <- run(FALSE)
  first_cal <- convert_NEONhdf5_to_POSIXct_time(h2o$cal_df$timeBgn[1])
  d18o <- h2o$wiso_subset_cal$`000_010_09m`$dlta18OH2o
  early <- convert_NEONhdf5_to_POSIXct_time(d18o$timeEnd) < first_cal
  expect_true(any(early))
  expect_true(all(is.na(d18o$mean_cal[early])))
  expect_true(all(is.na(d18o$min_cal[early])))
  expect_true(all(is.na(d18o$max_cal[early])))

  # extending the first calibration period calibrates (most of) those rows;
  # a few can still be removed by the median spike filter
  forced <- run(TRUE)$wiso_subset_cal$`000_010_09m`$dlta18OH2o
  expect_gt(mean(!is.na(forced$mean_cal[early])), 0.9)
  expect_false(isTRUE(all.equal(forced$mean_cal[early], d18o$mean[early])))
})

# ---- Arguments passed through calibrate_carbon() ---------------------------

# calibrate_carbon(gap_fill_parameters = TRUE) was accepted and documented but
# never passed to the ambient calibration functions, so it had no effect.
test_that("calibrate_carbon passes gap_fill_parameters to the ambient step", {
  seen <- NULL
  real <- NEONiso:::calibrate_ambient_carbon_gainoffset
  local_mocked_bindings(
    calibrate_ambient_carbon_gainoffset = function(...,
                                                   gap_fill_parameters = FALSE) {
      seen <<- c(seen, gap_fill_parameters)
      real(..., gap_fill_parameters = gap_fill_parameters)
    }
  )
  suppressWarnings(calibrate_carbon(fin, tempfile(), "ONAQ",
    method = "gainoffset", gap_fill_parameters = TRUE, write_to_file = FALSE))
  expect_gt(length(seen), 0)
  expect_true(all(seen))
})

# ---- Output files ------------------------------------------------------------

# The gain/offset table used to be written only for the deprecated method name
# "Bowling_2003"; the recommended "gainoffset" produced an empty calData group.
test_that("method = 'gainoffset' writes calGainsOffsets to calData", {
  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)
  suppressWarnings(calibrate_carbon(fin, fout, "ONAQ", method = "gainoffset",
                                    write_to_file = TRUE))
  expect_identical(
    NEONiso:::h5_ls_group(fout, "ONAQ/dp01/data/isoCo2/calData"),
    "calGainsOffsets")
})

# Build a minimal HDF5 file containing the given isoCo2 data groups.
make_isoco2_file <- function(path, groups) {
  fid <- NEONiso:::h5_create_file(path)
  for (g in c("ONAQ", "ONAQ/dp01", "ONAQ/dp01/data", "ONAQ/dp01/data/isoCo2")) {
    NEONiso:::h5_close_group(NEONiso:::h5_create_group(fid, g))
  }
  for (g in groups) {
    NEONiso:::h5_close_group(
      NEONiso:::h5_create_group(fid, paste0("ONAQ/dp01/data/isoCo2/", g)))
  }
  NEONiso:::h5_close(fid)
}

# validate_output_file() checks that every input group at the calibrated
# averaging interval appears in the output. It used to expect only 9-minute
# groups, so calibrate_carbon(write_to_file = TRUE) failed on every current
# NEON file, which uses 6-minute groups (#97). Current files also carry
# their own isoCo2/calData group, which was counted twice. The archive gas
# and other averaging intervals (e.g. 30m) are not written and must be
# ignored.
test_that("validate_output_file accepts current 6-minute files", {
  fin6 <- tempfile(fileext = ".h5")
  fout6 <- tempfile(fileext = ".h5")
  on.exit(unlink(c(fin6, fout6)), add = TRUE)
  make_isoco2_file(fin6, c("000_010_06m", "000_010_30m", "co2Low_06m",
                           "co2Low_30m", "co2Arch_06m", "calData"))
  make_isoco2_file(fout6, c("000_010_06m", "co2Low_06m", "calData"))
  expect_no_error(NEONiso:::validate_output_file(fin6, fout6, "ONAQ", "Co2",
                                                 avg_suffix = "06m"))
})

test_that("validate_output_file still catches a missing output group", {
  fin6 <- tempfile(fileext = ".h5")
  fout6 <- tempfile(fileext = ".h5")
  on.exit(unlink(c(fin6, fout6)), add = TRUE)
  make_isoco2_file(fin6, c("000_010_06m", "co2Low_06m", "calData"))
  make_isoco2_file(fout6, c("000_010_06m", "calData"))   # co2Low_06m missing
  expect_error(NEONiso:::validate_output_file(fin6, fout6, "ONAQ", "Co2",
                                              avg_suffix = "06m"),
               "Output file structure has diverged")
})
