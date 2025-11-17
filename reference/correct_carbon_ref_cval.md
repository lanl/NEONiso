# correct_carbon_ref_cval

This ugly function is present out of necessity, and will only exist for
as long as it is necessary. It is an internal correction within the
NEONiso calibration routines that is required as there are some
mismatches between the 'true' isotope reference values and those in the
NEON HDF5 files. NEON is working on correcting this, and after it has
been corrected, this function has no need to exist and will be
immediately deprecated. As a result, this function is fairly messy but
there is little incentive to improve it.

## Usage

``` r
correct_carbon_ref_cval(
  std_frame,
  site,
  omit_already_corrected = TRUE,
  co2_tol = 5,
  d13c_tol = 0.25
)
```

## Arguments

- std_frame:

  Standard data frame to perform swap on.

- site:

  NEON four letter site code.

- omit_already_corrected:

  Should we attempt correction, if it's already been corrected in the
  raw files.

- co2_tol:

  Tolerance to use to select co2 values that need to be replaced, in
  ppm. Default = 5 ppm.

- d13c_tol:

  Tolerance to use to select d13C values that need to be replaced, in
  ppm. Default = 0.25 per mil.

## Value

A data.frame, based on `std_frame`, where NEON-supplied reference values
have been corrected if a mismatch has previously been identified.

## Details

Current sites and time periods affected:

## Author

Rich Fiorella <rfiorella@lanl.gov>
