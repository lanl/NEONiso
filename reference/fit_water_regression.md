# Estimate slope/intercept of water isotope calibration regression

Performs regression between measured and known carbon isotope and mole
fractions to generate a transfer function and associated uncertainty
estimates using both 5-fold and leave-one-out cross-validation methods.
Regression occurs on d18O and d2H values.

## Usage

``` r
fit_water_regression(
  ref_data,
  calibration_half_width,
  slope_tolerance,
  r2_thres,
  plot_regression_data = FALSE,
  plot_dir = "/dev/null",
  site,
  min_nobs = NA
)
```

## Arguments

- ref_data:

  Reference data.frame from which to estimate calibration parameters.

- calibration_half_width:

  Determines the period (in days) from which reference data are selected
  (period is 2\*calibration_half_width).

- slope_tolerance:

  Allows for filtering of slopes that deviate from 1 by slope_tolerance.

- r2_thres:

  What is the minimum r2 value permitted in a 'useful' calibration
  relationship.

- plot_regression_data:

  True or false - should we plot the data used in the regression? Useful
  for debugging.

- plot_dir:

  If plot_regression_data is true, where should the plots be saved?

- site:

  Needed for regression plots.

- min_nobs:

  Minimum number of high-frequency observations to define a peak.

## Value

Returns a data.frame of calibration parameters. Output data.frame
includes slope, intercept, and r^2 values for d13C and CO2 values.
