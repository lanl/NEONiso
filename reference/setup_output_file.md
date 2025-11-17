# Structure a new HDF5 file

Creates a skeleton HDF5 file for the calibrated data, essentially
setting up the HDF5 groups at the /site/dp01/{data,ucrt,qfqm} level.

## Usage

``` r
setup_output_file(inname, outname, site, analyte)
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

## Value

Nothing to the environment, but creates a new data file with the most
basic output HDF5 structure consistent with NEON's data files.

## Author

Rich Fiorella <rfiorella@lanl.gov>
