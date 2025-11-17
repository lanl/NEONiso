# Extract only the data corresponding to validation/calibration time periods.

Extracts data matching a value of "h2oLow," "h2oMed," or "h2oHigh" which
correspond to the validation gases of known d18O, d2H that are fed to
the analyzer daily.

## Usage

``` r
extract_water_calibration_data(data_list)
```

## Arguments

- data_list:

  List containing data, from the /\*/dp01/data/ group in NEON HDF5 file.

## Value

Returns data frame of required variables.

## Author

Rich Fiorella <rfiorella@lanl.gov>
