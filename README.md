# NEONiso

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/188347333.svg)](https://zenodo.org/badge/latestdoi/188347333)
[![R-CMD-check](https://github.com/SPATIAL-Lab/NEONiso/workflows/R-CMD-check/badge.svg)](https://github.com/SPATIAL-Lab/NEONiso/actions)
[![codecov](https://codecov.io/gh/SPATIAL-Lab/NEONiso/branch/main/graph/badge.svg?token=ZHDFEU5NZW)](https://codecov.io/gh/SPATIAL-Lab/NEONiso)
<!-- badges: end -->

Author: Rich Fiorella \
Last Updated: February 18, 2021.

This repository contains an R package to calibrate NEON atmospheric isotope data. 
Please report any issues you have, bugs found, or enhancement suggestions as issues to this repository.

## Citation information:
A manuscript describing the carbon isotope calibration techniques used in this package has just been accepted at JGR-Biogeosciences (doi: [10.1029/2020JG005862](https://doi.org/10.1029/2020JG005862)). Users of this package should also cite the Zenodo DOI above. If you are interested in using code that should provide data identical to that in the JGR-Biogeosciences paper, please install version 0.3 of the package. 

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
Alternatively, you can install a specific version of the package (e.g., v0.1)
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

## Future plans
There will be two major changes and a minor change coming to this package in the next few months:
1) We are starting to work on calibration routines for the NEON water isotope products - more info soon.
2) When development of this package began, neonUtilities::stackEddy did not exist. As a result, we decided to apply calibration routines on monthly files only. The development of the stackEddy function opens up some new possibilities that allow for a more continuous calibration. As a result, we are working on functions that will take advantage of this capability.
3) (minor change) Unit tests will be added as we have time in order to improve code reliability. code coverage badge is provided for transparency, but this effort has just begun to spin up.

## DATA ALERT:
[NOTE: THIS SECTION IS OUT OF DATE AND WILL BE REVISED SHORTLY.]

Several months of data on the NEON data portal have an issue where the Picarro time clock has diverged from the valve manifold time. A fix has been developed, but has not been propagated to the NEON data portal. In the interim, corrected files are available here: https://www.dropbox.com/sh/i9a61g2crv26ess/AADFCT80TPeMz2ayfNsCotg8a?dl=0

