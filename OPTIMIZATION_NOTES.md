# NEONiso optimization notes

Baseline commit: `80d75a0`. Phase 1 (planning) only; no package code changed.

## Bugs found (logged, behavior left as-is)

**B1. `convert_POSIXct_to_NEONhdf5_time()` mishandles some POSIXct inputs.**
It calls `lubridate::ymd_hms()` on POSIXct input, which goes through
`as.character()`. R drops the time part for values exactly at midnight, so:
- `as.POSIXct("2019-05-01 00:00:00", tz = "UTC")` → `NA` with a
  "1 failed to parse" warning (an all-midnight vector warns "All formats failed
  to parse").
- Non-UTC input is relabelled rather than converted:
  `as.POSIXct("2019-05-01 03:00", tz = "America/Denver")` →
  `"2019-05-01T03:00:00.000Z"` (should be `09:00Z`).

Not triggered by the bundled fixture (stackEddy times are GMT and not at
midnight; `cal_df` times use `00:00:00.0001`), but it could affect real data
whose `timeBgn` falls exactly at 00:00:00.

**B2. `force_to_end` / `force_to_beginning` have no effect.** All three
`calibrate_ambient_*` functions assign to `caldf$end[nrow(caldf)]` and
`caldf$start[1]`, but the calibration data frame's columns are `timeEnd` /
`timeBgn`. That creates unused columns, so the interval assignment never sees
the forced endpoints.

**B3. `method = "gainoffset"` writes no calibration parameters to the HDF5
file.** `write_carbon_calibration_data()` writes `calGainsOffsets` only when
`method == "Bowling_2003"`, and `calRegressions` only for `"linreg"`. For
`"gainoffset"` (the recommended name) the `calData` group is created but left
empty, and `validate_output_file()` still passes because it only checks group
names. Confirmed on the ONAQ fixture: `calData` holds `calGainsOffsets` for
`"Bowling_2003"`, `calRegressions` for `"linreg"`, and is empty for
`"gainoffset"`.

**B4. `calculate_13CO2()` ignores its `f` argument** when calling
`calculate_12CO2(total_co2, delta13c)`, so a non-default `f` is applied
inconsistently. No effect at the default `f`.

**B5. One-second gap at each day boundary.** `fit_*_regression` builds daily
windows `[00:00:00.0001, 23:59:59.0000]`. An ambient row whose `timeEnd` falls
in `(23:59:59, 00:00:00.0001)` is matched to no window and stays uncalibrated.
Rare with NEON's irregular end times, but possible.

**B6. `calibrate_carbon(gap_fill_parameters = ...)` is accepted but never
passed** to `calibrate_ambient_carbon_*()`, so it has no effect.

**B7. `calibrate_carbon(write_to_file = TRUE)` fails on current NEON files.**
`validate_output_file()` keeps only `09m` CO2 groups from the input file
(`grepl("09m", ...)`). NEON moved isoCo2 to 6-minute averaging (see NEWS
0.7.0), so real files have `000_0x0_06m` / `co2*_06m` (plus `30m`) groups.
The input list then has only `calData`, the output has the `06m` groups, and
the function stops with "Output file structure has diverged from input
file!". The output file has already been fully written at that point.
Confirmed on ONAQ 2019-05 (RELEASE-2026) at baseline `80d75a0`. Only the
old 09m test fixture passes. `benchmarks/run_one.R` tolerates exactly this
error so W1/W2 can be timed with HDF5 output included.

**B8 (not this package): `neonUtilities::zipsByProduct()` 4.1.0** crashes
with "subscript out of bounds" in `checkUrlExp()` when a download hits R's
default 60 s `timeout`. `options(timeout = 3600)` avoids it.

## Change log

Status (2026-11-19): C1–C4 are implemented, one commit each, on branch
`perf-c1-c4`. All outputs are **bit-identical** to baseline `80d75a0` on W0,
W1 and W2 for gainoffset, linreg and water (R objects via `identical()`, HDF5
via `h5diff`). The full test suite passes (100 tests, 235 expectations) after
every commit.

**Timings are provisional.** The W1/W2 runs happened on battery power with the
lid closed, and macOS maintenance sleeps landed inside some runs (outliers of
50–2,700 s). Medians of 5 below are robust to the occasional outlier but are
not final. Raw data: `benchmarks/results/timings_session1_battery_UNRELIABLE.csv`.
`bench.sh` now refuses to run on battery and wraps runs in `caffeinate`.
Re-run on AC power before drawing final conclusions.

| Workload | Method | Baseline | C1 | +C3 | +C4 | +C2 (HEAD) | Peak RSS base → HEAD |
|---|---|---|---|---|---|---|---|
| W1 | gainoffset | 3.93 s | 3.09 | 3.01 | 3.13 | 2.88 (−27%) | 397 → 371 MB |
| W1 | linreg | 4.21 s | 3.21 | 3.11 | 3.06 | 2.92 (−31%) | 399 → 372 MB |
| W1 | water | 3.16 s | 2.72 | 2.75 | 2.50 | 2.33 (−26%) | 418 → 405 MB |
| W2 | gainoffset | 24.07 s | 20.82 | 19.31 | 18.87 | 18.73 (−22%) | 1285 → 1154 MB |
| W2 | linreg | 25.19 s | 22.88 | 21.09 | 21.15 | 19.69 (−22%) | 1346 → 1210 MB |
| W2 | water | 17.63 s | 16.64 | 14.58 | 13.98 | 13.51 (−23%) | 631 → 563 MB |

Per change (function-level numbers are clean in-process `bench::mark` runs):

- **C1 `16ca78f`, fast path in `convert_POSIXct_to_NEONhdf5_time`**:
  52 → 15 ms on 30k timestamps. Clear win on W0 (−12 to −16%, clean data) and
  W1. Keep.
- **C3 `56b8009`, `findInterval` assignment to calibration periods**:
  205 → 0.8 ms (365 periods × 30k rows). Consistent −1.5 to −2.1 s on W2 across
  all three methods; flat on W1, as expected. Keep (provisional).
- **C4 `4a965aa`, numeric per-day subset in `fit_*_regression`**: regression
  functions −51% (carbon) / −19% (water) in-process. At the workload level the
  regression loop is <1–5% of runtime: W1 water −0.29 s (Wilcoxon p = 0.01 in a
  10-run A/B), carbon and W2 within noise. **Undecided**: the rule is to keep a
  change only if it clearly beats noise, and the A/B was sleep-contaminated.
  Re-test on AC; revert if not clear.
- **C2 `8bb51d7`, vectorized rolling median**: 510 → 34 ms on 50k rows;
  0 mismatches vs zoo on 20,000 random vectors. W1 −5 to −8%. Keep
  (provisional). This is the least obvious code of the four (~35 lines in
  `rolling_median()`), so it's worth a look in review; it is commented, and
  falls back to zoo for anything unusual.

Not yet done: final AC-power timings, a final re-profile, remaining hotspots,
and the `git diff 80d75a0 -- tests/` confirmation (checked below, empty so far).
