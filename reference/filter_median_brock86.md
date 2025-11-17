# Apply a median absolute deviation filter

Median absolute deviation filter of Brock 1986, with user specified
width and magnitude thresholds.

## Usage

``` r
filter_median_brock86(data, width = 7, threshold = 5)
```

## Arguments

- data:

  Vector to filter.

- width:

  Width of filter, in rows.

- threshold:

  Only filter values that are `abs(threshold)` away from median

## Value

Returns filtered vector.

## Author

Rich Fiorella <rfiorella@lanl.gov>
