# Ingest and stack variables needed in calibration.

Opens and stacks isotope ratio and water/carbon dioxide mole fraction
variables from monthly HDF5 files. If a new enough version of
`neonUtilities` is available, this function will try to use `fasttime`
in order to accelerate data stacking.

## Usage

``` r
ingest_data(inname, analyte, name_fix = TRUE, amb_avg, ref_avg)
```

## Arguments

- inname:

  A file (or list of files) to extract data from for calibration.

- analyte:

  Carbon (Co2) or water (H2o)?

- name_fix:

  Fix to data frame required for next-generation calibration functions,
  but breaks old 'by_month()' functions. This parameter provides a
  necessary work around until these functions are removed.

- amb_avg:

  The averaging interval of the ambient data to extract.

- ref_avg:

  The averaging interval of the reference data to extract.

## Value

List of data frames, taken from files specified in `inname`

## Author

Rich Fiorella <rfiorella@lanl.gov>
