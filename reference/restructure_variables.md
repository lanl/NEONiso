# Restructures data.frames imported by ingest_data to shorten variable names and Wrapper function around restructure_carbon_variables and restructure_water_variables.

Restructures data.frames imported by ingest_data to shorten variable
names and Wrapper function around restructure_carbon_variables and
restructure_water_variables.

## Usage

``` r
restructure_variables(dataframe, varname, mode, group, species)
```

## Arguments

- dataframe:

  Input data.frame, from
  [`neonUtilities::stackEddy`](https://rdrr.io/pkg/neonUtilities/man/stackEddy.html)

- varname:

  Which variable are we applying this function to? There's a list of ~10
  common ones to write to the hdf5 file.

- mode:

  Are we fixing a reference data frame or an ambient data frame?

- group:

  Data, ucrt, or qfqm?

- species:

  Set to 'Co2' for carbon; 'H2o' for water

## Value

data.frame formatted for output to hdf5 file.

## Author

Rich Fiorella <rfiorella@lanl.gov>
