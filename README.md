# NEONiso

<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/SPATIAL-Lab/NEONiso.svg?branch=master)](https://travis-ci.org/SPATIAL-Lab/NEONiso)
  <!-- badges: end -->

Author: Rich Fiorella \
Last Updated: Jan 13, 2020.

## DATA ALERT:
A few notes about data quality issues that persist after calibration (and closing associated issues since I suspect these are unlikely to ever be fixed (they are acquisition issues, not calibration issues): 

1. do not use carbon isotope data from UNDE from 5/2019-9/2019 (inclusive), SRER from 7/2019, or TEAK from 8 and 9/2018. there are clear spectral issues that cannot be corrected by calibration

This repository contains functions for an R package to calibrate NEON atmospheric isotope data. Separate functions exist for calibrating carbon and water data.

Please report any issues you have, bugs found, or enhancement suggestions as issues to this repository.

## Installation instructions:

If you want to contribute to development:
1) Clone this repo to a local repository.
2) Open the .Rproj file in Rstudio.
3) Rstudio has the required package building functions within it to compile the package for use. I have a series of scripts as well that will use these functions to "drive" calibration of the NEON EC files.

If you just want to use the functions (note: this won't work quite yet - needs an auth token to be generated first!):
1) Install devtools.
2) Run: devtools::install_github("SPATIAL-Lab/NEONiso") Note: requires an auth token! Not yet implemetned.

## What functions are complete? (please test!)
### Calibration functions
1) **calibrate_carbon_Bowling2003** - calibrates carbon isotope data using the Bowling et al. 2003 AFM gain and offset calibration technique. Simultaneously corrects to the VPDB scale and for any non-linearities with CO2 concentration. Determined by Wen et al. 2013 to be "most accurate" carbon isotope calibration method. Includes logic to select two good calibration points, where available, to calculate gain and offset parameters and the third calibration point as a QA/QC point.  Ambient observations are calibrated by a call to a different function, calibrate_ambient_carbon_Bowling2003.

Median/mean site-level RMSE for calibration as of 2/13/20: 0.21/0.24‰.
Median/mean site-level RMSE for calibratino as of 2/17/20: 0.20/0.23‰.

2) **calibrate_carbon_linreg** - calibrates carbon isotope data using a standard linear regression. Corrects to VPDB scale. Currently uses all three points and an R^2 threshold to determine if calibration data are "good." Considering replacing with an alternate version with some *a priori* constraints on what constitutes a "quality" analysis, and using only two points to leave the third as a calibration point. Ambient observations are calibrated by a call to a different function, calibrate_ambient_carbon_linreg. Gain and offset parameters calculated by calculate_gain_and_offset

3) **calibrate_water_linreg** - calibrates water isotope data using a standard linear regression. Corrects to VSMOW scale. Currently uses all three points and an R^2 threshold to determine if calibration data are "good." Considering replacing with an alternate version with some *a priori* constraints on what constitutes a "quality" analysis, and using only two points to leave the third as a calibration point. Ambient observations are calibrated by a call to a different function, calibrate_ambient_water_linreg. Uses only the last three injections of each standard as a way to minimize memory effects; otherwise, does not attempt to correct for memory issues.

### "Helper" functions
Functions that help arrange NEON data but are not the calibration code:

1) terrestrial_core_sites - returns a vector of four-letter codes corresponding to NEON core terrstrial sites
2) terrestrial_relocatable_sites - returns a vector of four-letter codes corresponding to NEON relocatable terrestrial sites
3) copy_qfqm_group and copy_ucrt_group - functions to copy over qfqm and ucrt datasets from input h5 data to output data. These groups are unchanged, only the /SITE/dp01/data group is changed by the clibration functions.
4) create_h5_groups_for_calibrated_isotopes - helper function to mirror h5 group hierarchy of input files.
5) swap_standard_isotoperatios - **LIKELY TEMPORARY** - in late 2019, we noticed that one of the water standards seemed to have swapped d2H and d18O reference values. This function swaps them until it has been fixed and new data provisioned by NEON.
6) convert_POSIXct_to_NEONhdf5_time - NEON timestamps in the HDF5 files follow a specific convention - this function converts back from an R POSIXct object to a character vector in the NEON format.

## What's next?
As of 1/13/20, these functions are all operational and can produce a first version of calibrated isotope data. Issuing the first code release as a result. Future code development needs to target the following tasks:
1) propagation of uncertainty through the calibration functions (this is 80% done for calibrate_carbon_Bowling2003, but for no other functions).
2) despite effort to make flexible code, calibration functions still produce nonsense values ~5% of the time. Determine causes, report, and develop workarounds as required.
3) low humidity correction not implemented! data should be coming available from NEON in the next few months to begin to implement this correction. caution for any water data produced by these codes where H2O < 5000 ppm is advised.
