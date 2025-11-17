# Estimate slope/intercept of carbon isotope calibration regression

Performs regression between measured and known carbon isotope and mole
fractions to generate a transfer function and associated uncertainty
estimates using both 5-fold and leave-one-out cross-validation methods.
Regression occurs either on 12CO2/13CO2 mole fractions (gainoffset
method) or on the CO2 and d13C values (linreg).

## Usage

``` r
fit_carbon_regression(
  ref_data,
  method,
  calibration_half_width,
  plot_regression_data = FALSE,
  plot_dir = "/dev/null",
  site,
  min_nobs = NA
)
```

## Arguments

- ref_data:

  Reference data.frame from which to estimate calibration parameters.

- method:

  Are we using the gain-and-offset method ("gainoffset"), formerly
  called the Bowling et al. 2003 method in this package, or direct
  linear regression of d13C and CO2 mole fractions ("linreg")?

- calibration_half_width:

  Determines the period (in days) from which reference data are selected
  (period is 2\*calibration_half_width).

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

Returns a data.frame of calibration parameters. If
`method == "gainoffset"`, then data.frame includes gain and offset
parameters for 12CO2 and 13CO2, and r^2 values for each regression. If
`method == "linreg"`, then data.frame includes slope, intercept, and r^2
values for d13C and CO2 values.

## Author

Rich Fiorella <rfiorella@lanl.gov>
