# Convert NEON HDF5 file time to POSIXct

Converts the date time string in NEON HDF5 files to a POSIXct object for
use in R.

## Usage

``` r
convert_NEONhdf5_to_POSIXct_time(intime)
```

## Arguments

- intime:

  Vector of datetimes in NEON data files (as string) to convert to
  POSIXct class

## Value

Vector of datetimes from NEON data file now in POSIXct format.

## Author

Rich Fiorella <rfiorella@lanl.gov>

## Examples

``` r
convert_NEONhdf5_to_POSIXct_time("2019-06-01T12:00:00.000Z")
#> [1] "2019-06-01 12:00:00 GMT"
```
