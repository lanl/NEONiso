# Validate output file.

Function ensures that the output file has the correct groups in it, as a
check to ensure proper file structure at the end of the calibration
routines.

## Usage

``` r
validate_output_file(inname, outname, site, analyte, avg_suffix = NULL)
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

- avg_suffix:

  Averaging-interval suffix(es) of the input groups that were
  calibrated, e.g. "06m". Input groups with other suffixes are not
  expected in the output. If NULL, "09m" for carbon and c("03m", "09m")
  for water.

## Value

Nothing to environment, simply checks to make sure expected groups are
in output.

## Author

Rich Fiorella <rfiorella@lanl.gov>
