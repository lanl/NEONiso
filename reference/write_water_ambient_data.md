# Write calibrated ambient water isotope ratio observations to file.

Write out ambient observations from the NEON EC towers where the isotope
data have been calibrated using this package.

## Usage

``` r
write_water_ambient_data(outname, site, amb_data_list)
```

## Arguments

- outname:

  Output file name.

- site:

  NEON 4-letter site code.

- amb_data_list:

  Calibrated list of ambient data - this is the output from one of the
  calibrate_ambient_water\* functions.

## Value

Nothing to the environment, but writes data in amb_data_list to file.

## Author

Rich Fiorella <rfiorella@lanl.gov>
