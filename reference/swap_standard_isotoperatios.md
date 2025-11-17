# swap_standard_isotoperatios

There are a few suspected instances where the water isotope ratios for
oxygen and hydrogen have been flipped in the reference data. This
function corrects them until they are corrected in the NEON database
using a d-excess filter.

## Usage

``` r
swap_standard_isotoperatios(std_frame, dxs_thres = 500)
```

## Arguments

- std_frame:

  Standard data frame to perform swap on.

- dxs_thres:

  d-excess threshold to indicate when to swap.

## Value

A data.frame based on `std_frame`, where d18O and d2H values have been
swapped from NEON input files if determined to have a reference value
mismatch. Mismatch is determined based on the d-excess of the standard
(= d2H - 8\*d18O), using a value of 500 by default.

## Author

Rich Fiorella <rfiorella@lanl.gov>
