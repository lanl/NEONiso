# Select validation data corresponding to a particular day

Select validation data corresponding to a particular day

## Usage

``` r
select_daily_reference_data(standard_df, analyte, min_nobs = NA)
```

## Arguments

- standard_df:

  Input reference data.frame.

- analyte:

  Are we calibrating CO2 and H2O? (Use argument 'co2' or 'h2o', or else
  function will throw error)

- min_nobs:

  Minimum number of high-frequency observations to define a peak. If not
  supplied, defaults are 200 for `analyte = 'co2'` or 30 for
  `analyte = 'h2o'`

## Value

Smaller data.frame where only the reference data selected to use in the
calibration routines is returned. Assumes that we are calibrating on a
daily basis, and not on a longer time scale. Data are selected based on
two criteria: cannot be missing, and must be at least a certain number
of high-frequency observations in order to qualify as a valid
measurement. For the water system, this function also keeps only the
last three injections for each reference water per day.
