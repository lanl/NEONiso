# Restructure ingested variables for the carbon isotope system.

Restructures carbon isotope measurement system variables and shortens
names to simplify referencing variables elsewhere in calibration code.

## Usage

``` r
restructure_carbon_variables(dataframe, varname, mode, group)
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

## Value

data.frame formatted for output to hdf5 file.
