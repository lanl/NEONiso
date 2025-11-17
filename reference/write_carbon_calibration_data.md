# Write carbon calibrations to file

Write a `data.frame` with slope, intercepts, and error estimates of
calibrations for carbon isotope system. If `gainoffset` method was used
the slopes/intercepts are called gain/offsets for each isotopologue.

## Usage

``` r
write_carbon_calibration_data(outname, site, cal_df, method, to_file = TRUE)
```

## Arguments

- outname:

  Output file name.

- site:

  NEON 4-letter site code.

- cal_df:

  Calibration data frame - this is the output from fit_carbon_regression

- method:

  Was the Bowling et al. 2003 or the linear regression method used in
  fit_carbon_regression?

- to_file:

  Write to file (TRUE) or to environment (FALSE).

## Value

Nothing to the environment, but writes out the calibration parameters
(e.g., gain and offset or regression slopes and intercepts) to the
output hdf5 file.

## Author

Rich Fiorella <rfiorella@lanl.gov>
