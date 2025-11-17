# Return heavy-to-light isotope ratio of primary standard.

Returns the heavy-to-light isotope ratio of the dominant standard for
that element. Vienna Standard Mean Ocean Water (VSMOW) for oxygen and
hydrogen isotopes, Vienna Pee Dee Belemnite (VPDB) for carbon stable
isotopes.

## Usage

``` r
get_Rstd(element)
```

## Arguments

- element:

  Which element to return standard ratio - carbon, oxygen, or hydrogen.

## Value

Heavy-to-light isotope ratio of most common stable isotope standard.
VSMOW for water, VPDB for carbon.

## Author

Rich Fiorella <rfiorella@lanl.gov>

## Examples

``` r
get_Rstd("carbon") # returns 0.0111797
#> [1] 0.0111797
get_Rstd("oxygen") # returns 2005.20e-6
#> [1] 0.0020052
```
