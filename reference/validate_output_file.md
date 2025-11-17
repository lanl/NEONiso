# Validate output file.

Function ensures that the output file has the correct groups in it, as a
check to ensure proper file structure at the end of the calibration
routines.

## Usage

``` r
validate_output_file(inname, outname, site, analyte)
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

Nothing to environment, simply checks to make sure expected groups are
in output.

## Author

Rich Fiorella <rfiorella@lanl.gov>
