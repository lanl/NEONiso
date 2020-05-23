# NEONiso

<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/SPATIAL-Lab/NEONiso.svg?branch=master)](https://travis-ci.org/SPATIAL-Lab/NEONiso) [![DOI](https://zenodo.org/badge/188347333.svg)](https://zenodo.org/badge/latestdoi/188347333)
  
  <!-- badges: end -->

Author: Rich Fiorella \
Last Updated: May 23, 2020.

## DATA ALERT:
A few notes about data quality issues that persist after calibration (and closing associated issues since I suspect these are unlikely to ever be fixed (they are acquisition issues, not calibration issues): 

1. do not use carbon isotope data from UNDE from 5/2019-9/2019 (inclusive), SRER from 7/2019, or TEAK from 8 and 9/2018. there are data issues that cannot be corrected by calibration

This repository contains functions for an R package to calibrate NEON atmospheric isotope data. Separate functions exist for calibrating carbon and water data.

Please report any issues you have, bugs found, or enhancement suggestions as issues to this repository.

## Installation instructions:
1) Install devtools.
2) Run: devtools::install_github("SPATIAL-Lab/NEONiso")
