# Converts delta value to heavy-to-light isotope ratio

Converts a delta value (in per mil) to the heavy-to-light isotope ratio.

## Usage

``` r
delta_to_R(delta_values, element)
```

## Arguments

- delta_values:

  A vector of isotope ratios in delta notation.

- element:

  Which element to return R values - carbon, oxygen, or hydrogen.

## Value

Vector of isotope ratios (R values).

## Author

Rich Fiorella <rfiorella@lanl.gov>

## Examples

``` r
delta_to_R(delta_values = 0, element = 'oxygen') # 2005.2e-6 for VSMOW.
#> [1] 0.0020052
```
