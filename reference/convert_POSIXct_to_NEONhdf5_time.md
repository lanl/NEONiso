# Convert a POSIXct object to the format used in NEON HDF5 files

Converts a POSIXct object back to the character format used by NEON in
their HDF eddy covariance files. Output format, using strptime syntax,
is %Y-%m-%dT%H:%M:%OSZ.

## Usage

``` r
convert_POSIXct_to_NEONhdf5_time(intime)
```

## Arguments

- intime:

  POSIXct vector to convert to NEON time format.

## Value

Returns character version of POSIXct object matching NEON time variable
format.

## Author

Rich Fiorella <rfiorella@lanl.gov>

## Examples

``` r
convert_POSIXct_to_NEONhdf5_time(Sys.time())
#> [1] "2025-11-17T18:44:20.000Z"
```
