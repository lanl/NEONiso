# Make plots of carbon calibration data for debugging

Makes plots of carbon calibration data regressions, primarily for
debugging and validation purposes.

## Usage

``` r
carbon_regression_plots(caldata, plot_filename, method, mtitle)
```

## Arguments

- caldata:

  Data frame corresponding to a specific calibration period.

- plot_filename:

  What should the output file name for diagnostic plot be?

- method:

  Which method are we using? Currently works for gain/offset.

- mtitle:

  Fed from above routine - what should the plot title be?

## Value

Nothing to the environment, but a pdf plot to a file.

## Author

Rich Fiorella <rfiorella@lanl.gov>
