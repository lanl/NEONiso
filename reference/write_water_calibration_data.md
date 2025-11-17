# Write water calibration parameters to file

Write a `data.frame` with slope, intercepts, and error estimates of
calibrations for water isotope system.

## Usage

``` r
write_water_calibration_data(outname, site, cal_df)
```

## Arguments

- outname:

  Output file name.

- site:

  NEON 4-letter site code.

- cal_df:

  Calibration data frame - this is the output from fit_water_regression

## Value

Nothing to the environment, but writes out the calibration parameters
(e.g., regression slopes and intercepts) to the output hdf5 file.

## Author

Rich Fiorella <rfiorella@lanl.gov>
