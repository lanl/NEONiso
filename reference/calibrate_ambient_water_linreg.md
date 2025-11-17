# calibrate_ambient_water_isotopes

Function called by `calibrate_ambient_water_linreg` to apply slope and
intercept parameters to the ambient datasets (000_0x0_09m and
000_0x0_30m) to correct to the VSMOW scale. This function should
generally not be used independently, but should be used with
`calibrate_ambient_water_linreg`. Note that in this version *NO
CORRECTION FOR HUMIDITY* is performed. Use with caution.

## Usage

``` r
calibrate_ambient_water_linreg(
  amb_data_list,
  caldf,
  outname,
  site,
  filter_data = TRUE,
  force_to_end = TRUE,
  force_to_beginning = TRUE,
  r2_thres = 0.9
)
```

## Arguments

- amb_data_list:

  List containing ambient d18O/d2H datasets. Will include all variables
  in 000_0x0_xxm. (character)

- caldf:

  Calibration data frame containing slope and intercept values for d18O
  and d2H values.

- outname:

  Output variable name. Inherited from `calibrate_ambient_water_linreg`

- site:

  Four-letter NEON code corresponding to site being processed.

- filter_data:

  Apply a median filter to output ambient data? inherited.

- force_to_end:

  In given month, calibrate ambient data later than last calibration,
  using the last calibration? (default true)

- force_to_beginning:

  In given month, calibrate ambient data before than first calibration,
  using the first calibration? (default true)

- r2_thres:

  Minimum r2 value for calibration to be considered "good" and applied
  to ambient data.

## Value

Nothing to environment; returns calibrated ambient observations to the
output file. This function is not designed to be called on its own.

## Author

Rich Fiorella <rfiorella@lanl.gov>
