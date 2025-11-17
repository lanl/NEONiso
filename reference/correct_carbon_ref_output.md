# Correct carbon ref output

Corrects known mismatches in the database where standard values do not
actually match what they should in data files per calVal measurements.

## Usage

``` r
correct_carbon_ref_output(
  std_list,
  site,
  omit_already_corrected = TRUE,
  co2_tol = 5,
  d13c_tol = 0.25,
  ref_gas
)
```

## Arguments

- std_list:

  List containing reference/validation gas measurements.

- site:

  Four-letter NEON site code.

- omit_already_corrected:

  Skip correction if the reference gas values have already been
  corrected in the files (default TRUE) If you have older versions of
  the files, you may want to set this to FALSE.

- co2_tol:

  Tolerance used to identify a mismatch in CO2 values. Will correct
  measured CO2 values within +/- co2_tol within time period identified
  as having incorrect reference values.

- d13c_tol:

  Tolerance used to identify a mismatch in d13C values. Will correct
  measured d13C values within +/- d13c_tol within time period identified
  as having incorrect reference values.

- ref_gas:

  Which reference gas is being corrected? Expects "co2High", "co2Med",
  or "co2Low"

## Value

A version of std_list with corrected reference values.

## Author

Rich Fiorella <rfiorella@lanl.gov>
