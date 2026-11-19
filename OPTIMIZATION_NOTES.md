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

(Phase 2: one entry per kept/reverted optimization.)
