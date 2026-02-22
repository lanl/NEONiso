# Structure a new HDF5 file

Creates a skeleton HDF5 file for the calibrated data, essentially
setting up the HDF5 groups at the /site/dp01/{data,ucrt,qfqm} level.

## Usage

``` r
setup_output_file(
  inname,
  outname,
  site,
  analyte,
  attrs = NULL,
  keep_open = FALSE
)
```

## Arguments

- inname:

  Input file name.

- outname:

  Output file name.

- site:

  NEON 4-letter site code.

- analyte:

  Carbon ('Co2') or water ('H2o') system?

- attrs:

  Pre-read attributes list from the input file. If NULL (default),
  attributes are read from `inname`.

- keep_open:

  If TRUE, return the open file handle instead of closing it. The caller
  is responsible for closing via `h5_close()`.

## Value

If `keep_open = TRUE`, returns the open HDF5 file handle. Otherwise
nothing (creates a new data file with basic HDF5 structure consistent
with NEON's data files).

## Author

Rich Fiorella <rfiorella@lanl.gov>
