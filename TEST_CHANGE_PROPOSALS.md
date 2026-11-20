# Test change proposals

Each proposal needs approval before anything under `tests/` changes.

| # | What | Status |
|---|---|---|
| P1 | Direct tests for `filter_median_brock86()` | **Applied** (`42eae01`, approved) |
| P2 | Update 3 carbon ambient snapshots/expectations for the B2 fix | **Applied** (approved) |
| P3 | Regression tests for the bug fixes | **Applied** (approved, revised) as `tests/testthat/test-calibration_edge_cases.R` |
| P4 | Update water ambient snapshot/expectations for the B9 fix | **Applied** (approved) |

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

## P3. Regression tests for the bug fixes — applied

Applied, after approval, as `tests/testthat/test-calibration_edge_cases.R`.
It was revised from the draft so that it stands alone: each test documents the
old behaviour it guards against, and refers to GitHub issues (#66, #97, #105)
rather than to these notes. It also adds a test for the B9 fix (water rows
outside calibration periods are NA, and `force_cal_to_beginning` fills them).
All tests except one fail on the pre-fix commit `d7e0e36`; the exception
checks that character input is still parsed as UTC, which did not change.
B10 is not covered, since it needs a 30-minute fixture.

## P4. Update water ambient snapshot and expectations for the B9 fix — applied

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
