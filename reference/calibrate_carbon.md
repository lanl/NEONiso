# Calibrate NEON carbon isotope data using validation data sets.

**\[experimental\]** This function drives a workflow that reads in NEON
carbon isotope data of atmospheric CO2, calibrates it to the VPDB scale,
and (optionally) writes the calibrated data to a new HDF5 file. Two
different approaches are possible: a) a calibration on 12CO2 and 13CO2
isotopologues independently, after Bowling et al. 2003 (Agr. For. Met.),
or b) a direct calibration of d13C and CO2 values using linear
regression. Most of the time the results generated are extremely similar
to each other. Wen et al. 2013 compared several different carbon isotope
calibration techniques and found this to be the superior method under
most circumstances. We also found this to be the case for NEON data
(Fiorella et al. 2021; JGR-Biogeosciences).

## Usage

``` r
calibrate_carbon(
  inname,
  outname,
  site,
  method = "Bowling_2003",
  calibration_half_width = 0.5,
  force_cal_to_beginning = TRUE,
  force_cal_to_end = TRUE,
  gap_fill_parameters = FALSE,
  filter_ambient = TRUE,
  r2_thres = 0.95,
  correct_ref_data = TRUE,
  write_to_file = TRUE,
  remove_known_bad_months = TRUE,
  plot_regression_data = FALSE,
  plot_directory = NULL,
  avg = 6,
  min_nobs = NA,
  standards = c("co2Low", "co2Med", "co2High")
)
```

## Arguments

- inname:

  Input file(s) that are to be calibrated. If a single file is given,
  output will be a single file per site per month. If a list of files
  corresponding to a timeseries at a given site is provided, will
  calibrate the whole time series.

- outname:

  Name of the output file. (character)

- site:

  Four letter NEON site code for site being processed. (character)

- method:

  Are we using the Bowling et al. 2003 method ("Bowling_2003") or direct
  linear regression of d13C and CO2 mole fractions ("linreg")?

- calibration_half_width:

  Determines the period (in days) from which reference data are selected
  (period is 2\*calibration_half_width).

- force_cal_to_beginning:

  Extend first calibration to the beginning of the file? (default true)

- force_cal_to_end:

  Extend last calibration to the end of the file? (default true)

- gap_fill_parameters:

  Should function attempt to 'gap-fill' across a bad calibration by
  carrying the last good calibration forward? Implementation is fairly
  primitive currently, as it only carries the last known good
  calibration that's available forward rather than interpolating, etc.
  Default FALSE.

- filter_ambient:

  Apply the median absolute deviation filter (Brock 86) to remove
  impulse spikes in output ambient data? (logical; default true)

- r2_thres:

  Minimum r2 threshold of an "acceptable" calibration. Acts to remove
  calibration periods where a measurement error makes relationship
  nonlinear. Default = 0.95

- correct_ref_data:

  NEON has indicated there are a few instances where reported d13C or
  CO2 reference values are wrong. If set to true, correct known
  incorrect values. This argument will (hopefully, eventually) go away
  after NEON has fixed the reference database. Users will be warned
  prior to removal of this argument.

- write_to_file:

  Write calibrated ambient data to file? (Mostly used for testing)

- remove_known_bad_months:

  There are a few site months with known spectral issues where the
  isotope ratios are likely unrecoverable. This parameter allows removal
  of these files, but allows them to remain in archive.

- plot_regression_data:

  Default false; this is useful for diagnostics.

- plot_directory:

  Only used if plot_regression_data is TRUE, but specify where to write
  out diagnostic plot of regression data.

- avg:

  The averaging interval to extract, in minutes. Default 6.

- min_nobs:

  Minimum number of high-frequency observations to define a peak.

- standards:

  Which reference gases (standards) to use? Default is all, but can pass
  a subset of "co2Low", "co2Med", and "co2High" as a vector to this
  argument as well.

## Value

Returns nothing to the environment, but creates a new output HDF5 file
containing calibrated carbon isotope values.

## Details

The 'linreg' method simply takes measured and reference d13C and CO2
values and generates a transfer function between them using
[`lm()`](https://rdrr.io/r/stats/lm.html). For the gain-and-offset
method, d13C and CO2 values are converted to 12CO2 and 13CO2 mole
fractions. Gain and offset parameters are calculated for each
isotopologue independently, and are analogous to regression slope and
intercepts, but jointly correct for CO2 concentration dependence and
place d13C values on the VPDB scale. The gain and offset parameters are
defined by:

\$\$G = (X\_{2,ref}-X\_{1,ref})/(X\_{2,meas}-X\_{1,meas})\$\$ \$\$O =
X\_{2,ref}- G X\_{2,meas}\$\$ Calibrated ambient isotopologues are then
given as: \$\$X\_{cal} = X\_{meas} G + O\$\$

Measurements of reference materials were considered "good" if the
following conditions were met:

- Measured CO2 concentrations were within 10 ppm of known "reference"
  concentrations.

- Variance of the CO2 concentration in standard peak was \< 5 ppm.

- Measured d13C value must be within 5 per mil of known "reference" d13C
  value.

The first two criteria are intended to filter out periods where there is
a clear issue with the gas delivery system (i.e., nearly empty gas tank,
problem with a valve in the manifold, etc.); the third criterion was
adopted after visual inspection of data timeseries revealed that often
the first standard measurement following an instrument issue had
higher-than-expected error. This criterion clips clearly poor values.
Selection of these criteria will become a function argument, and
therefore customizable, in a future release.

The behavior of this function will be a bit different depending on what
is supplied as `inname`. If a single file is provided, the output will
be monthly. However, a list of files corresponding to a site can also be
provided, and then a single output file per site will be generated.

## Author

Rich Fiorella <rfiorella@lanl.gov>

## Examples

``` r
if (FALSE) fin <- system.file('extdata',
'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.20201020T211037Z.packed.h5',
         package = 'NEONiso', mustWork = TRUE)
calibrate_carbon_bymonth(inname = fin, outname = 'out.h5',
         site = 'ONAQ', write_to_file = FALSE)
#> Error in calibrate_carbon_bymonth(inname = fin, outname = "out.h5", site = "ONAQ",     write_to_file = FALSE): could not find function "calibrate_carbon_bymonth"
calibrate_carbon_bymonth(inname = fin, outname = 'out.h5',
         site = 'ONAQ', method = 'linreg', write_to_file = FALSE) # \dontrun{}
#> Error in calibrate_carbon_bymonth(inname = fin, outname = "out.h5", site = "ONAQ",     method = "linreg", write_to_file = FALSE): could not find function "calibrate_carbon_bymonth"
```
