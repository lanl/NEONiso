# Extract only the data corresponding to validation/calibration time periods.

Extracts data matching a value of "co2Low," "co2Med," or "co2High" which
correspond to the validation gases of known CO2, d13C that are fed to
the analyzer daily.

## Usage

``` r
extract_carbon_cal_data(
  data_list,
  standards = c("co2Low", "co2Med", "co2High")
)
```

## Arguments

- data_list:

  List containing data, from the /\*/dp01/data/ group in NEON HDF5 file.

- standards:

  Which reference gases (standards) to use? Default is all, but can pass
  a subset of "co2Low", "co2Med", and "co2High" as a vector to this
  argument as well.

## Value

Returns data frame of required variables.

## Author

Rich Fiorella <rfiorella@lanl.gov>
