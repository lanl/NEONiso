# Produce estimates of the calibration error.

Estimate calibration error using a 5-fold cross-validation. A 5-fold
cross-validation was chosen as each calibration window should have at
least 6 data points (e.g., if only daily validation data are used for
the calibration) and therefore this ensures that the cross-validation
should always run. Model is fit using `lm` and the `caret` package, with
root-mean-square error (RMSE), the R-squared value, and mean-absolute
error (MAE) extracted from the cross-validation.

## Usage

``` r
estimate_calibration_error(formula, data)
```

## Arguments

- formula:

  Formula to pass to caret::train to perform cross validation.

- data:

  Data frame to perform cross-validation on.

## Author

Rich Fiorella <rfiorella@lanl.gov>
