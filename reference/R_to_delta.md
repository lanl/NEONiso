# Convert heavy-to-light isotope ratio to delta values.

Converts a heavy-to-light stable isotope ratio to a corresponding delta
value, in per mil values.

## Usage

``` r
R_to_delta(R_values, element)
```

## Arguments

- R_values:

  A vector of isotope ratios (e.g., R values).

- element:

  Which element to return delta values - carbon, oxygen, or hydrogen.

## Value

Vector of isotope ratios in delta notation.

## Author

Rich Fiorella <rfiorella@lanl.gov>

## Examples

``` r
R_to_delta(R_values = 2005.20e-6, element = 'oxygen') # returns 0.
#> [1] 0
```
