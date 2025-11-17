# Write calibrated carbon ambient data to file

Write out ambient observations from the NEON EC towers where the isotope
data (either H2O or CO2) have been calibrated using this package.

## Usage

``` r
write_carbon_ambient_data(outname, site, amb_data_list, to_file = TRUE)
```

## Arguments

- outname:

  Output file name.

- site:

  NEON 4-letter site code.

- amb_data_list:

  Calibrated list of ambient data - this is the output from one of the
  calibrate_ambient_carbon\* functions.

- to_file:

  Write to file (TRUE) or to environment (FALSE).

## Value

Nothing to the environment, but writes data in amb_data_list to file.

## Author

Rich Fiorella <rfiorella@lanl.gov>
