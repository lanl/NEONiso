# Test change proposals

Each proposal needs approval before anything under `tests/` changes.

| # | What | Status |
|---|---|---|
| P1 | Direct tests for `filter_median_brock86()` | **Applied** (`3b5f4bc`, approved) |
| P2 | Update 3 carbon ambient snapshots/expectations for the B2 fix | **Applied** (approved) |
| P3 | New `test-bug_fixes.R`: regression tests for B1–B7 | Proposed |
| P4 | Update water ambient snapshot/expectations for the B9 fix | Proposed |

## P1. Direct tests for `filter_median_brock86()` (and `rolling_median()`) — applied

**Why:** C2 replaced the `zoo::rollapply(median)` call inside
`filter_median_brock86()` with a vectorized rolling median. The function has
no direct test; it is only covered indirectly through the calibration
snapshots, which use one small fixture with few NA patterns. The equivalence
script `benchmarks/check_c2_median_filter.R` (20,000 random vectors, 0
mismatches) is outside the test suite.

**Proposed new file** `tests/testthat/test-filtering_functions.R`:

```r
zoo_reference <- function(data, width = 7, threshold = 5) {
  filt <- zoo::rollapply(data, width, median, na.rm = TRUE, fill = NA)
  spikes <- abs(data - filt) > threshold
  data[spikes == TRUE] <- NA
  data
}

test_that("filter_median_brock86 removes isolated spikes", {
  x <- c(rep(-8, 10), 30, rep(-8, 10))
  out <- filter_median_brock86(x)
  expect_true(is.na(out[11]))
  expect_identical(out[-11], x[-11])
})

test_that("filter_median_brock86 matches zoo reference on NA-heavy data", {
  set.seed(1)
  for (p_na in c(0, 0.1, 0.5, 0.9, 1)) {
    x <- rnorm(500, -8, 3)
    x[runif(500) < p_na] <- NA
    x[sample(500, 5)] <- 40
    expect_identical(filter_median_brock86(x), zoo_reference(x))
  }
})

test_that("filter_median_brock86 handles short and special inputs", {
  expect_identical(filter_median_brock86(c(1, 2, 3)), c(1, 2, 3))
  expect_identical(filter_median_brock86(numeric(0)), numeric(0))
  x <- c(NaN, Inf, -Inf, rnorm(20), NA)
  expect_identical(filter_median_brock86(x), zoo_reference(x))
})
```

Keeping `zoo` as the reference uses an existing import and needs no new
dependency.

## P2. Update carbon ambient snapshots for the B2 fix — applied

**Why:** with `force_cal_to_end = TRUE` (the default) now working, the
fixture's last ambient row (2019-05-31 00:01, after the last calibration
day) is calibrated with that day's parameters instead of being NA
(gainoffset) or raw (linreg). That is the only change in the fixture
output, and it fails 3 tests on branch `fix-b1-b7`:

- `test-gold_file_regression.R`: "carbon gainoffset ambient matches
  baseline" and "carbon linreg ambient matches baseline". Fix: re-record
  those two snapshots (`testthat::snapshot_accept("gold_file_regression")`
  after a run); the cal_df and water snapshots are unchanged.
- `test-regression_snapshots.R` "calibrate_carbon gainoffset produces stable
  ambient d13C values": the two `730` counts become `731` (lines 52 and 57).
  Means and SDs stay within the existing tolerances (d13C -9.2737 / 0.5026;
  CO2 418.23 / 8.025).

## P3. Regression tests for the bug fixes

**Why:** each fix (B1-B7) has no test that would catch it regressing. All
of these pass on `fix-b1-b7` and fail (or, for B6, error) on the pre-fix
commit `54ed293`, checked by running the file from outside `tests/`. B10
(`avg >= 10`) is not covered: it needs a 30-minute fixture.

**Proposed new file** `tests/testthat/test-bug_fixes.R`:

```r
# Regression tests for bugs B1-B7 (see OPTIMIZATION_NOTES.md).
# B10 (avg >= 10) needs a 30-minute fixture, which the package does not ship.

fin <- system.file("extdata",
                   "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
                   package = "NEONiso", mustWork = TRUE)

# ---- B1: time conversion ----

test_that("convert_POSIXct_to_NEONhdf5_time handles midnight and time zones", {
  midnight <- as.POSIXct("2019-05-01 00:00:00", tz = "UTC")
  expect_no_warning(out <- convert_POSIXct_to_NEONhdf5_time(midnight))
  expect_identical(out, "2019-05-01T00:00:00.000Z")
  denver <- as.POSIXct("2019-05-01 03:00:00", tz = "America/Denver")
  expect_identical(convert_POSIXct_to_NEONhdf5_time(denver),
                   "2019-05-01T09:00:00.000Z")
  expect_identical(convert_POSIXct_to_NEONhdf5_time("2019-05-01 03:00:00"),
                   "2019-05-01T03:00:00.000Z")
})

# ---- B4: f is used consistently ----

test_that("calculate_13CO2 passes f through to calculate_12CO2", {
  f <- 0.01
  expect_equal(calculate_13CO2(400, -8, f = f) + calculate_12CO2(400, -8, f = f),
               400 * (1 - f))
})

# ---- B5: daily windows leave no gap ----

test_that("daily calibration windows are contiguous at ms resolution", {
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
    expect_equal(bgn %% 86400, rep(0, n))               # starts at midnight
    expect_true(all(bgn[-1] - end[-n] > 0))             # no overlap
    expect_true(all(bgn[-1] - end[-n] <= 0.0011))       # gap <= 1 ms
  }
})

# ---- B2: force_cal_to_end / force_cal_to_beginning ----

test_that("force_cal_to_end calibrates ambient data after the last cal day", {
  run <- function(force) {
    suppressWarnings(calibrate_carbon(fin, tempfile(), "ONAQ",
      method = "gainoffset", calibration_half_width = 0.5,
      force_cal_to_end = force, force_cal_to_beginning = force,
      write_to_file = FALSE))
  }
  on <- run(TRUE)$ciso_subset_cal$`000_010_09m`$dlta13CCo2
  off <- run(FALSE)$ciso_subset_cal$`000_010_09m`$dlta13CCo2
  last <- nrow(on)
  # the fixture's last ambient row ends after the last calibration day
  expect_true(is.na(off$mean_cal[last]))
  expect_false(is.na(on$mean_cal[last]))
  expect_identical(on$mean_cal[-last], off$mean_cal[-last])
})

# ---- B6: gap_fill_parameters reaches the ambient functions ----

test_that("calibrate_carbon forwards gap_fill_parameters", {
  seen <- NULL
  real <- NEONiso:::calibrate_ambient_carbon_gainoffset
  local_mocked_bindings(
    calibrate_ambient_carbon_gainoffset = function(..., gap_fill_parameters = FALSE) {
      seen <<- c(seen, gap_fill_parameters)
      real(..., gap_fill_parameters = gap_fill_parameters)
    }
  )
  suppressWarnings(calibrate_carbon(fin, tempfile(), "ONAQ",
    method = "gainoffset", gap_fill_parameters = TRUE, write_to_file = FALSE))
  expect_true(length(seen) > 0 && all(seen))
})

# ---- B3: gainoffset writes its calibration table ----

test_that("gainoffset writes calGainsOffsets to calData", {
  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)
  suppressWarnings(calibrate_carbon(fin, fout, "ONAQ", method = "gainoffset",
                                    write_to_file = TRUE))
  expect_identical(NEONiso:::h5_ls_group(fout, "ONAQ/dp01/data/isoCo2/calData"),
                   "calGainsOffsets")
})

# ---- B7: validation of 06m files that carry their own calData ----

make_iso_file <- function(path, groups) {
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

test_that("validate_output_file accepts current 06m files", {
  fin6 <- tempfile(fileext = ".h5")
  fout6 <- tempfile(fileext = ".h5")
  on.exit(unlink(c(fin6, fout6)), add = TRUE)
  make_iso_file(fin6, c("000_010_06m", "000_010_30m", "co2Low_06m",
                        "co2Low_30m", "co2Arch_06m", "calData"))
  make_iso_file(fout6, c("000_010_06m", "co2Low_06m", "calData"))
  expect_no_error(NEONiso:::validate_output_file(fin6, fout6, "ONAQ", "Co2",
                                                 avg_suffix = "06m"))
  # a missing group is still caught
  make_iso_file(fout6, c("000_010_06m", "calData"))
  expect_error(NEONiso:::validate_output_file(fin6, fout6, "ONAQ", "Co2",
                                              avg_suffix = "06m"),
               "Output file structure has diverged")
})
```

## P4. Update water ambient snapshot and expectations for the B9 fix

**Why:** with B9 fixed (approved: unmatched rows become NA),
`calibrate_water()` at its defaults (`force_cal_to_* = FALSE`) leaves the
fixture's May 1–6 ambient rows, which come before the first calibration
period (May 7), as NA instead of raw values. That fails 2 tests:

- `test-gold_file_regression.R` "water ambient matches baseline": re-record
  that snapshot (`testthat::snapshot_accept("gold_file_regression")`); the
  water cal_df and the carbon snapshots are unchanged.
- `test-regression_snapshots.R` "calibrate_water produces stable ambient
  isotope values" (lines 136–143):

| Expectation | Old | New |
|---|---|---|
| d18O non-NA `mean_cal` | 915 | 696 |
| d18O mean | -22.988 | -24.186 |
| d18O sd | 4.431 | 4.283 |
| d2H non-NA `mean_cal` | 848 | 639 |
| d2H mean | -176.16 | -185.48 |
| d2H sd | 32.72 | 31.65 |

The means move by more than the tolerances because the removed raw values
were biased relative to the calibrated ones.
