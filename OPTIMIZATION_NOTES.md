# NEONiso optimization notes

Baseline commit: `80d75a0`. Final commit: see `git log perf-c1-c4`.

## Bugs found

Found during the optimization and left as-is there. B1–B7, B9 and B10
were then fixed, with approval, on branch `fix-b1-b7` (one commit each, on
top of `perf-c1-c4`); B8 is upstream. Details of each
fix are in its commit message and in "Bug fixes" below.

| Bug | Status | Commit |
|---|---|---|
| B1 midnight / time-zone conversion | fixed | `eea9dec` |
| B2 `force_to_*` no effect | fixed (changes 3 carbon snapshots, see P2) | `22ecdf1` |
| B3 gainoffset writes no calData | fixed | `3d7da4e` |
| B4 `calculate_13CO2` ignores `f` | fixed | `f52c98a` |
| B5 1 s gap at day boundary | fixed | `555b802` |
| B6 `gap_fill_parameters` not forwarded | fixed | `0a7a5a4` |
| B7 carbon validation fails on 06m files | fixed | `3f13527` |
| B8 neonUtilities timeout | upstream, not ours | – |
| B9 unmatched ambient rows keep raw values (water, linreg) | fixed: now NA; filling them tracked in #105 (changes water snapshot, see P4) | `6e5d426` |
| B10 `avg >= 10` gives `030m` group names | fixed | `18763cf` |


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

**B9. Uncalibrated ambient rows keep their raw values (water, carbon
linreg).** `calibrate_ambient_water_linreg()` and
`calibrate_ambient_carbon_linreg()` start `mean_cal` (and min/max) as a copy
of the raw values and overwrite only the rows matched to a calibration
period. Rows outside every period (e.g. with `force_cal_to_end = FALSE`, the
water default) are written as "calibrated" but hold raw values. The
gainoffset path starts from NA instead. Fixed: unmatched rows are now NA
(decision: NA, with the calibration extension options to fill them later;
issue #105).

**B10. Averaging suffix is not zero-padded correctly.** `ingest_data()`
built group names as `paste0("0", avg, "m")`, so `avg = 30` gave
`000_010_030m`, which matches no NEON group, and
`calibrate_carbon(avg = 30, write_to_file = TRUE)` failed validation. Found
while checking the B7 write route. Fixed with `sprintf("%02dm")`.

## Bug fixes (branch `fix-b1-b7`)

Each fix: full suite run, and W0/W1 outputs (W2 at the end) compared with
the pre-fix outputs, row by row where they differ.

| Fix | Output change on the workloads |
|---|---|
| B1 | W0/W1 none. W2 water: one `h2oMed_03m` row starting exactly at 2019-03-17 00:00:00 had `timeBgn` written as NA; now correct. |
| B2 | W0 carbon: the last ambient row (2019-05-31 00:01, after the last calibration day) is now calibrated (was NA for gainoffset, raw for linreg). W0 water, W1, W2: none. |
| B3 | gainoffset files gain `calData/calGainsOffsets`; nothing else changes. |
| B4, B6 | None at defaults (B6: no W0/W1 day is below `r2_thres`; passthrough checked with a trace). |
| B5 | None on W0/W1/W2 (no ambient end time falls in the old gap). |
| B9 | W0 water: the May 1–6 rows before the first calibration period go from raw values to NA (219 d18O, 209 d2H). W0 linreg, W1, W2: none. |
| B7, B10 | None at defaults. Carbon `write_to_file = TRUE` now succeeds on W1/W2, and with `avg = 30`. |

**B7 write route, end to end.** Validation only compares group names, so
`benchmarks/check_write_roundtrip.R` checks the whole route: every data
frame returned with `write_to_file = FALSE`, plus `cal_df`, must be in the
written file with identical values, with no extra groups or variables.
Passes for W0 (exercising `ingest_data()`'s 9-minute fallback) and W1 (06m)
for all three methods. Two extra causes turned up along the way: current
files carry their own `isoCo2/calData` group, which was counted twice, and
B10. The `qfqm`/`ucrt` groups in the output are created empty, as before.

**Tests.** 103 tests (100 + P1). B2 changed three carbon ambient snapshot
tests; P2 (approved, applied) updates them. B9 changes two water tests; P4
proposes the update and is not yet applied, so those 2 tests fail on
`fix-b1-b7`. P3 (regression tests for B1–B7) is proposed, not applied.

**Speed.** B1 replaces the C1 fast path with a direct `format()` for all
POSIXct input: 15.6 vs 16.3 ms on 30k timestamps, 3.2 vs 5.2 MB allocated
(identical output). The other fixes add O(1) work per call. A full W1/W2
re-benchmark is pending (machine was on battery; `bench.sh` refuses).
`benchmarks/check_c1_time_conversion.R` checked C1 against the old
`ymd_hms()` behavior, so it now reports the intended B1 differences.

## Results (final, branch `perf-c1-c4`)

Kept: **C1, C3, C2**. Tried and reverted: **C4**. All outputs are
**bit-identical** to baseline `80d75a0` on W0, W1 and W2 for gainoffset,
linreg and water (18 comparisons: R objects via `identical()`, HDF5 via
`h5diff`). Full test suite: 100 tests, 235 expectations, 0 failures/skips,
after every commit.

Environment: R 4.6.2, macOS arm64, hdf5r backend, AC power, `caffeinate`.
Fresh `Rscript` per run, `set.seed(1)`, baseline and final interleaved, 5
runs each. Time = in-process `calibrate_*()` call (median, IQR); peak memory =
max RSS of the process (median). Raw data: `benchmarks/results/timings.csv`
(labels `final_base`, `final`; per-commit labels `baseline`, `c1`, `c3`, `c4`,
`c2`). In all 9 cases below the baseline and final timing ranges do not
overlap (Wilcoxon p = 0.012, the minimum for 5 vs 5).

### W0: bundled ONAQ 2019-05 fixture

| Method | Baseline | Final | Change | Peak RSS baseline → final |
|---|---|---|---|---|
| gainoffset | 1.905 s (IQR 0.080) | 1.482 s (0.085) | **−22.2%** | 335 → 276 MB (−17.5%) |
| linreg | 1.938 s (0.064) | 1.461 s (0.028) | **−24.6%** | 336 → 277 MB (−17.4%) |
| water | 1.786 s (0.062) | 1.344 s (0.079) | **−24.7%** | 318 → 274 MB (−13.9%) |

### W1: one real month (ONAQ 2019-05, 127 MB, 4 heights)

| Method | Baseline | Final | Change | Peak RSS baseline → final |
|---|---|---|---|---|
| gainoffset | 3.868 s (0.136) | 2.957 s (0.123) | **−23.6%** | 396 → 372 MB (−6.1%) |
| linreg | 3.930 s (0.301) | 3.097 s (0.080) | **−21.2%** | 397 → 374 MB (−5.9%) |
| water | 3.174 s (0.080) | 2.498 s (0.112) | **−21.3%** | 419 → 405 MB (−3.4%) |

### W2 / W3: twelve real months (ONAQ 2019, 1.2 GB), time and peak memory

| Method | Baseline | Final | Change | Peak RSS baseline → final (W3) |
|---|---|---|---|---|
| gainoffset | 25.219 s (1.293) | 19.198 s (0.300) | **−23.9%** | 1315 → 1152 MB (−12.4%) |
| linreg | 26.107 s (0.679) | 20.302 s (0.499) | **−22.2%** | 1346 → 1216 MB (−9.6%) |
| water | 18.235 s (0.316) | 13.626 s (0.415) | **−25.3%** | 632 → 662 MB (**+4.7%**, see below) |

**W2 water peak memory.** This is the one case where peak RSS went up. It is
consistent (final 660–664 MB vs baseline 626–633 MB over 5 runs each) and
appears only in the C1+C3+C2 combination: C1+C3 alone peaks at about 580 MB,
and C1+C3+C4+C2 at about 560 MB. R's own peak accounting (`gc()` max used)
shows vector data peaking *below* baseline in every version (final 120 MB vs
baseline 143 MB). The extra is in Ncells, i.e. language-object garbage not yet
collected when the peak happened. So this is GC timing, not more live data.
The most likely mechanism (not proven) is that C2's per-window `vapply()` for
even-count windows adds small garbage, and without C4's removal of dplyr
garbage the collector runs later. Flagged for your
decision (see "Open decisions").

## Change log (incremental effect, clean AC-power matrix)

Medians, each version vs the previous one; W1 n = 5–10, W2 n = 5–8 per
version; p = Wilcoxon rank-sum.

### Kept

- **C1 `7853e0b`: fast path in `convert_POSIXct_to_NEONhdf5_time()`.**
  `format()` directly when the input is unnamed UTC/GMT POSIXct with whole
  seconds (1970–2100) and none at exact midnight; otherwise the original
  `ymd_hms()` path. Function: 52 → 15 ms (30k timestamps), 3.3× less
  allocation. Workloads: W1 −10 to −18% (p ≈ 0.01), W2 carbon −8 to −11%
  (p = 0.01), W2 water −3% (p = 0.30). Checked by
  `benchmarks/check_c1_time_conversion.R` (values *and* warnings across 16
  input classes, including the fallback ones).
- **C3 `df71ef9`: `calperiod_indices()` assigns ambient rows to calibration
  periods via `findInterval()`.** Used when the periods are finite,
  non-negative and disjoint under `%within%`'s own arithmetic; otherwise the
  original loop. Function: 205 → 0.8 ms and 336 → 3.7 MB allocated (365
  periods × 30k rows). Workloads: all 6 cases faster, W1 −3 to −8% (water
  p = 0.03), W2 −3 to −10% (water p = 0.02). Checked by
  `benchmarks/check_c3_calperiod_indices.R`.
- **C2 `d68763e`: vectorized rolling median (`rolling_median()`) in
  `filter_median_brock86()`.** One `order()` call sorts every window; the
  odd-count median is read directly and the even-count one uses `mean()`, as
  `median()` does; unusual inputs fall back to zoo. Function: 510 → 34 ms
  (50k rows). Workloads: W2 −9 to −10% (p ≤ 0.03), W1 linreg/water −8%
  (p < 0.05), W1 gainoffset −2% (p = 0.36). Checked by
  `benchmarks/check_c2_median_filter.R` (20,000 random vectors with
  NA/NaN/±Inf/ties, 0 mismatches). **Flag:** this is the least obvious code
  added (~35 lines). It is commented, but deserves a careful review read.

### Tried and reverted

- **C4 `18e664e` (reverted in `ab161e1`): numeric per-day subset in
  `fit_*_regression()`.** Bit-identical, and halves `fit_carbon_regression()`
  in-process (70 → 35 ms per month; water 146 → 118 ms). But the regression
  loop is under 5% of a whole run at default settings, and the
  interleaved A/B on AC power (C3 vs C3+C4) showed W1 +1.1/−0.7/−1.8% and W2
  +2.6/−2.1/−6.5%, none distinguishable from noise (p ≥ 0.13), so it was
  reverted per the rules. It could matter for runs with a large
  `calibration_half_width` (e.g. the `100000` used in `workflows/`), which W2
  does not exercise; see also its memory interaction above.

### Not attempted

- **C5 (leaner 5-fold CV):** not approved for this batch. With C4 reverted,
  the regression loop is 3% (carbon) and 9% (water) of W2. Not worth the
  floating-point risk at that share.

## Remaining hotspots (final build, W2 profile)

| Stage | gainoffset | water | What it would take |
|---|---|---|---|
| `neonUtilities::stackEddy` | **79%** | **79%** | External. Water calls it twice (9 m and 3 m averaging), reading every file twice. Options: an upstream change, or caching/parallel reads, both of which need your approval (rule 5). |
| `restructure_variables` (dplyr) | 6.7% | 3.3% | Times are now converted once per *variable* per height (about 15×/height); converting once per height would need a restructure of the exported `restructure_variables()` (an API/design change). |
| HDF5 ambient writes | 6.6% | 4.9% | hdf5r's per-dataset type inference; changing it means changing on-disk types. |
| `fit_*_regression` | 3.2% | 8.7% | C4 (reverted) + C5; low share at default half-widths. |
| `convert_POSIXct_to_NEONhdf5_time` | 4.1% | 1.3% | Residual fast-path cost; small. |

## Open decisions

1. **W2 water peak memory (+4.7% RSS, GC timing).** (a) Accept as is
   (recommended: live vector data is below baseline, and every other case uses
   10–17% less memory); or (b) restore C4, which in combination brings it to
   about 560 MB (−11%) but has no measurable time gain.
2. **TEST_CHANGE_PROPOSALS.md P1** (direct tests for the median filter).
3. ~~Bugs B1–B7, B9~~: fixed on `fix-b1-b7` (plus B10). Still open: the
   P3/P4 test changes, and #105 (filling rows outside calibration periods).

## Confirmation

`git diff 80d75a0 -- tests/` is empty (0 lines) at the final commit.

## Rerunning

```sh
# data: 12 ONAQ 2019 monthly files under $NEONISO_BENCH_DATA
#       (default ~/NEONiso-benchmark-data); W0 needs no download.
benchmarks/bench.sh <pkg_tree> <label> 5 W1 gainoffset linreg water
Rscript benchmarks/summarize.R <reference_label> <label1>,<label2>
Rscript benchmarks/run_one.R <pkg_tree> W2 water save <out_dir>
Rscript benchmarks/compare_outputs.R <baseline_out_dir> <candidate_out_dir>
Rscript benchmarks/profile_workload.R . W2 gainoffset
```
