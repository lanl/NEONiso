# NEONiso performance optimization — Phase 1 plan

Baseline commit: `80d75a0` (main, clean). No package code has been changed.

> **Status:** approved and carried out on branch `perf-c1-c4` (C1, C3, C2 kept;
> C4 reverted). Final results are in `OPTIMIZATION_NOTES.md`. This file is the
> original Phase 1 plan, kept as written.

## 0. Status / what's been done

- **Test suite baseline (R 4.6.2, arm64, hdf5r backend):** 100 tests,
  235 expectations, 0 failed / 0 skipped / 0 errors / 0 warnings; ~66 s wall.
- Missing dependencies (`zoo`, `lubridate`, `neonUtilities`, `ggplot2`,
  `gridExtra`, `hdf5r`, `fasttime`, `bench`) were installed into an isolated
  library in the session scratchpad, **not** into the Homebrew site-library.
- **No formal workload baseline yet.** The reference workloads were left blank,
  so section 1 proposes some for approval. The numbers in section 2 come from an
  *exploratory* Rprof run on the bundled fixture plus synthetic micro-benchmarks,
  only to rank hotspots.

## 1. Proposed reference workloads (need approval)

Important: the bundled `inst/extdata` files are **heavily reduced test
fixtures**. The ONAQ 2019-05 file has 1 tower height, 1,079 ambient rows and 24
rows per reference gas, and a full calibration runs in ~1.5 s. Real monthly
files have 4–8 heights and many more rows. Profiling only the fixture would
under-weight everything that scales with data size, so I'd like real data for
W1/W2.

| ID | What | Call | Why |
|----|------|------|-----|
| **W0** (smoke) | Bundled ONAQ 2019-05 fixture | `calibrate_carbon(..., method="gainoffset")`, `calibrate_carbon(..., method="linreg")`, `calibrate_water(...)`, `write_to_file=TRUE` | Fast; same file as the snapshot tests. Run after every change. |
| **W1** (typical) | One full real monthly DP4.00200.001 *basic* file, ONAQ 2019-05 | same three calls | One site-month, the unit of most production runs. |
| **W2** (stress / site-level) | 12 real monthly files, ONAQ 2019-01…12, passed together | same three calls, default `calibration_half_width` | Matches the site-level mode in `workflows/test_workflows.R` (tests 4/6/8). This is where the O(days × rows) costs show up. |
| **W3** (memory) | Peak RSS of W2 in a fresh `Rscript` process (`/usr/bin/time -l`) | — | Memory is most likely to matter for site-level runs. |

Protocol: fresh `Rscript` per measurement, `set.seed(1)` before each call
(the 5-fold CV columns are random), ≥5 runs, report median and IQR. Outputs
are saved with `saveRDS` (`write_to_file=FALSE` run) and the HDF5 files are
compared dataset by dataset.

**Data source:** W1/W2 need real files. Either mount the `T7 Shield` archive
used by `workflows/test_workflows.R` (currently not mounted), or I download ~12
ONAQ months from the public NEON API with `neonUtilities::zipsByProduct()`.

## 2. Hotspots (exploratory)

### 2a. Measured on W0 fixture (Rprof, 5 ms sampling; % of total wall time)

| Stage | carbon gainoffset | carbon linreg | water | Notes |
|---|---|---|---|---|
| `ingest_data` total | 60% | 52% | 50% | includes the next three rows |
| ↳ `convert_POSIXct_to_NEONhdf5_time` (via `lubridate::ymd_hms`) | **23%** | **18%** | **19%** | Called 2× per variable per height in `restructure_*_variables`. Most of the cost is lubridate's format *guessing* (`guess_formats`, `regexpr`). |
| ↳ `neonUtilities::stackEddy` | 21% | 23% | 23% | External package. Water calls it twice (9 m + 3 m). |
| ↳ dplyr pipelines in `restructure_*` (excl. time conversion) | ~5% | ~9% | ~8% | |
| HDF5 output (`write_*_ambient_data`, hdf5r `guess_dtype`/`create_dataset`) | 22% | 20% | 18% | Per-dataset hdf5r overhead |
| `.hdf5_backend` first call (loads hdf5r namespace) | 10% | — | — | One-time per session |
| `fit_*_regression` | 5% | 8% | 11% | small on fixture; scales with days |
| `calibrate_ambient_*` (incl. median filter) | 6% | 10% | 13% | small on fixture; scales with rows × days |
| `gc` (self) | 8% | 15% | 17% | allocation churn spread across the above |

### 2b. Scaling (synthetic micro-benchmarks, to project W1/W2)

| Function | Cost model | At W2-like size |
|---|---|---|
| `convert_POSIXct_to_NEONhdf5_time` | ~1.9 µs/element + fixed per-call guessing overhead; `format()` alone is ~0.5 µs/element | 30k timestamps: 56 ms → 16 ms. Called ~30× per height |
| `filter_median_brock86` (`zoo::rollapply(median)`) | ~10 µs/element, linear | 50k rows: 0.49 s; called 3–6× per height/reference list |
| Ambient interval assignment (`which(t %within% interval)` loop) | O(days × rows) | 365 days × 30k rows: 0.19 s per call; called once per height/reference (2× for water) |
| Regression loop, per calibration day | lm 0.12 ms, summary 0.15, loocv 0.17, **5-fold CV 1.14 ms**, plus 2 dplyr filters | ~3–5 ms/day → ~1–2 s per 365 days per method |

Projected ranking for W1/W2: the time conversion and median filter scale with
rows and should dominate, followed by the interval loop, then the regression
loop. stackEddy and HDF5 writes stay large but are mostly outside this package's
control. I'll confirm this on real data before changing anything.

## 3. Candidate optimizations

All of C1–C4 are designed to give **bit-identical** results: they change how
indices and strings are computed, not floating-point arithmetic. None needs a
rule-5 approval (no dependencies, compiled code, parallelism, persistent caches,
or meaningful memory increase).

| # | Change | Est. gain | Risk to results | Test coverage | Rule 5? |
|---|---|---|---|---|---|
| **C1** | **Fast path in `convert_POSIXct_to_NEONhdf5_time`**: if the input is POSIXct in UTC/GMT, every non-NA value is a whole second, and none is exactly at midnight, return `format(intime, "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC")` directly. Otherwise keep the existing `ymd_hms` path unchanged. The guards exist because the current path has quirks that must be preserved (see Notes B1). | ~3.5× on this function → **~15–20% of W0 runtime**, more on W1/W2 | Low: output is a string, and the fast path only applies where it is provably identical; everything else (midnight, sub-second, non-UTC, character input, warnings) takes the old path | Direct (`test-utility_functions.R`) + snapshots | No |
| **C2** | **Vectorize filtering in `filter_median_brock86`**: build the 7-wide window matrix, sort all windows with one `order()` call, and pick the middle element directly for odd non-NA counts. For even counts (only when a window has NAs) call base `mean()` on the two middle values so the arithmetic is exactly `median()`'s. Fall back to `zoo` for non-double input or length < 7. | ~10–50× on the filter → large share of W2 | Medium (edge cases: NA/NaN, ±Inf, all-NA windows, ends). I'd verify against the `zoo` result on thousands of random vectors (script in `benchmarks/`). | **No direct test**, only indirect via snapshots → I'd propose one in `TEST_CHANGE_PROPOSALS.md` first | No |
| **C3** | **Interval assignment via `findInterval`** in the 3 `calibrate_ambient_*` functions (4 sites): when calibration intervals are sorted, non-overlapping and well-formed (always true for `fit_*_regression` output), compute the same `var_inds_in_calperiod` index list in O(rows · log days). Otherwise use the existing loop. Downstream code is untouched, so results are identical by construction. | O(D·N) → O(N log D); ~0.2 s per height per site-year | Low–medium (closed-interval endpoints and NA times must match `%within%` exactly; I'd verify by comparing index lists) | Indirect (`test-data_regression.R`, snapshots) | No |
| **C4** | **Per-day subset in `fit_*_regression`**: replace `dplyr::filter(timeBgn %within% interval)` with a base-R logical index that does `%within%`'s arithmetic exactly; precompute `as.numeric(timeBgn)` once. Same rows, same order → same `lm` inputs, same RNG draws. | ~1–2 ms/day of ~4 ms → ~25–40% of the regression loop | Low | Direct (`test-data_regression.R`) + snapshots | No |
| C5 *(conditional)* | Leaner 5-fold CV in `estimate_calibration_error` using `lm.fit` and a direct `X %*% beta` instead of `lm()`/`predict()`. Must keep the same `sample()` calls. | ~2–3× on CV ≈ 30–40% of the regression loop | **Medium–high**: coefficients should be bit-identical (same QR), but `predict.lm` rank-deficiency handling and warnings would need replicating. I'd only keep it if outputs are bit-identical on W0–W2 and the code stays readable; otherwise revert and log. | No direct test of `estimate_calibration_error` | No (but float-sensitive) |

Order: C1 → C3 → C4 → C2 → (C5). Cheapest and safest first, then re-profile on
W1/W2 before C2/C5 because the ranking will shift.

## 4. Looks slow but not worth touching (for now)

- **`neonUtilities::stackEddy` (~21–23%)**: external. The water path reads
  files twice (9 m and 3 m averaging); merging those reads would need a
  different stackEddy API. Out of scope.
- **HDF5 writes (~18–22%)**: the cost is hdf5r's per-dataset type inference
  and compound-type creation. Reducing it would mean changing on-disk types or
  layout, which is a change to results.
- **`.hdf5_backend()` first call (~10% of W0)**: one-time namespace load,
  already cached for the session.
- **Redundant `group_by(yr, mn, dom)` before a row-wise `filter` in
  `restructure_carbon_variables` (reference branch)**: removing it changes the
  return class (tibble vs data.frame) of an exported function's output, and the
  gain is a few ms per month.
- **Converting `timeBgn`/`timeEnd` once per height instead of once per
  variable in `ingest_data`**: real (~15× fewer conversions) but needs
  restructuring `restructure_variables` (exported). With C1 in place the
  remaining gain is small, so I'd skip it unless W2 says otherwise.
- **`out$col[i] <- value` writes into a data.frame inside the regression
  loops**: about 12 µs each, negligible next to `lm`.

## 5. Decisions needed before Phase 2

1. Approve / amend workloads W0–W3, and choose the data source for W1/W2
   (mount drive vs. download from NEON).
2. Floating-point tolerance: I recommend **bit-for-bit**. C1–C4 are designed
   to achieve it, and C5 would be kept only if it does too.
3. Approve candidates C1–C4 (and whether to try C5).
4. Where to install the missing dependencies durably so you can rerun
   `benchmarks/`: your Homebrew site-library, or keep them isolated.

Bugs found while reading the code are logged in `OPTIMIZATION_NOTES.md`; none
have been changed.
