# NEONiso

<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/SPATIAL-Lab/NEONiso.svg?branch=master)](https://travis-ci.org/SPATIAL-Lab/NEONiso) [![DOI](https://zenodo.org/badge/188347333.svg)](https://zenodo.org/badge/latestdoi/188347333)
  
  <!-- badges: end -->

Author: Rich Fiorella \
Last Updated: December 1, 2020.

This repository contains an R package to calibrate NEON atmospheric isotope data. 
Please report any issues you have, bugs found, or enhancement suggestions as issues to this repository.

## Citation information:
A manuscript describing this package is currently under review. Citation information will
be updated when the paper has been released. In the interim, users of this package must
cite the Zenodo DOI above.

Please also check to ensure that you are compliant with NEON's data citation policy for any
products derived from this package: https://www.neonscience.org/data/about-data/data-policies

## Installation instructions:
1) You will need the rhdf5 package, which is not on CRAN. rhdf5 is available from bioconductor using:
```R
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("rhdf5")
```
2) Install devtools, which is available on CRAN.
3) Install NEONiso from GitHub. Development version can be installed using:
```R
devtools::install_github("SPATIAL-Lab/NEONiso")
```
Alternatively, you can install a specific version of the pacakge (e.g., v0.1)
by specifying the version tag:
```R
devtools::install_github("SPATIAL-Lab/NEONiso@v0.1")
```

## Usage:

Currently, this package has two "workhorse" functions to calibrate NEON atmospheric carbon
isotope data released in the Eddy Covariance Bundled Data Product (NEON DP4.00200.001). These functions are:
1) calibrate_carbon_Bowling2003, which applies a gain-and-offset calibration approach on 12CO2 and 13CO2 
isotopologues independently. (recommended calibration technique for NEON atmospheric carbon isotopes)
2) calibrate_carbon_linreg, which applies a simple OLS regression to d13C and CO2 mole fractions.

These functions are meant to be applied to a list or vector of uncalibrated data files, and produce output hdf5 files
that have (currently) only the CO2 and d13C variables instead of the entire data bundle. Development was targeted and 
tested on monthly basic files, but the functions should also work on the extended data files.

neonUtilities:::stackEddy *should* work on these output files - please file an issue if it does not.

Vignettes will be developed in the near future to provide a more explicit example of data workflows using this package.
In the interim, the function documentation for the above functions should help users get started.

## DATA ALERT:
[NOTE: THIS SECTION IS OUT OF DATE AND WILL BE REVISED SHORTLY.]

A few notes about data quality issues that persist after calibration-and closing associated issues since I suspect these are unlikely to ever be fixed as they are acquisition issues, not calibration issues: 

1. do not use carbon isotope data from: UNDE from 5/2019-9/2019 (inclusive), SRER from 7/2019, or TEAK from 8 and 9/2018, JORN from 11/2018-07/2019, KONA prior to 12/2017, and 8/2018-3/2019 for MOAB. there appears to be an issue with the manifold.
2. Per communications with NEON staff, there may be an issue with data timestamps for HARV (10/2018-07/2019), RMNP (01/2018-07/2018), and NIWO (04/2019-05/2019). The data appear to be high-quality, but their timestamps may be incorrect.
3. There appears to be a small but persistent long-term trend in the early portions of the record at SRER and ONAQ. It is not currently clear if this is a real feature, or a data issue - this will be updated in the future as the record is more complete.
4. Several cases exist where there is a prolonged high error in a standard's calibrated d13C and/or CO2 mole fraction. These issues have been raised with NEON, and in some cases may reflect a mismatching between the EC data files and the CalVal reference material database. The current code should automatically remove bad reference values - therefore, this issue shouldn't severely impact current data quality - but we expect that future reprocessing of the NEON EC data will lead to a reduction in sites/periods affected by this issue.

