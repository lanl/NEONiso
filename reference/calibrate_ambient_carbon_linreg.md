# Calibrate ambient carbon isotope data using linear regression

Function called by `calibrate_ambient_carbon_linreg` to apply gain and
offset parameters to the ambient datasets (000_0x0_09m and 000_0x0_30m).
This function should generally not be used independently, but should be
used with `calibrate_ambient_carbon_linreg`.

## Usage

``` r
calibrate_ambient_carbon_linreg(
  amb_data_list,
  caldf,
  site,
  filter_data = TRUE,
  force_to_end = TRUE,
  force_to_beginning = TRUE,
  gap_fill_parameters = FALSE,
  r2_thres = 0.9
)
```

## Arguments

- amb_data_list:

  List containing an ambient d13C dataset. Will include all variables in
  000_0x0_xxm. (character)

- caldf:

  Calibration data frame containing gain and offset values for 12C and
  13C isotopologues.

- site:

  Four-letter NEON code corresponding to site being processed.

- filter_data:

  Apply median absolute deviation filter from Brock 86 to remove impulse
  spikes? Inherited from `calibrate_ambient_carbon_linreg`

- force_to_end:

  In given month, calibrate ambient data later than last calibration,
  using the last calibration? (default true)

- force_to_beginning:

  In given month, calibrate ambient data before than first calibration,
  using the first calibration? (default true)

- gap_fill_parameters:

  Should function attempt to 'gap-fill' across a bad calibration by
  carrying the last good calibration forward? Implementation is fairly
  primitive currently, as it only carries the last known good
  calibration that's available forward rather than interpolating, etc.
  Default FALSE.

- r2_thres:

  Minimum r2 value for calibration to be considered "good" and applied
  to ambient data.

## Value

Nothing to environment; returns calibrated ambient observations to the
function orchestrating calibration (calibrate_carbon). This function is
not designed to be called on its own, and is not exported to the
namespace.

## Author

Rich Fiorella <rfiorella@lanl.gov>
