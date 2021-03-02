# setup_testing_data.R
# rich fiorella

rm(list=ls())

library(rhdf5)
# a script to setup data to be bundled with the NEONiso package.
# exists in rbuildignore, so should not be bundled with package.
# requires install and restart w/ NEONiso.

master_file <- "/Volumes/GradSchoolBackup/NEON/DP4_00200_001/WREF/NEON.D16.WREF.DP4.00200.001.nsae.2019-06.basic.20201020T180548Z.h5"
master_cal_file <- "~/Desktop/NEONcal/201109_CisoEC/WREF/NEON.D16.WREF.DP4.00200.001.nsae.2019-06.basic.20201020T180548Z.calibrated.h5"

#---------------------------------------
# extract co2 data for unit testing:
#---------------------------------------
ciso <- rhdf5::h5read(master_file, "/WREF/dp01/data/isoCo2")
ucrt <- rhdf5::h5read(master_file, "/WREF/dp01/ucrt/isoCo2")

# from raw file (and quick processing scripts), we need three datasets:
# 1) raw calibration data
# 2) raw calibration data after filtering
# 3) raw ambient data.

# 1) get raw calibration data.
co2RawRefData <- ciso[c("co2High_09m","co2Med_09m","co2Low_09m")]
save(co2RawRefData, file = "tests/testthat/co2RawRefData.Rdata")

# 1b) get raw calibration ucrt.
co2RawRefUcrt <- ucrt[c("co2High_09m","co2Med_09m","co2Low_09m")]
save(co2RawRefUcrt, file = "tests/testthat/co2RawRefUcrt.Rdata")

# 2) get calibration data after filtering.
co2FiltRefData <- extract_carbon_calibration_data(ciso, NULL, "high")
save(co2FiltRefData, file = "tests/testthat/co2FiltRefData.Rdata")

# 3) get raw ambient data - currently from a monthly file, might be better to get from daily file.
co2RawAmbData <- ciso$`000_010_09m`
save(co2RawAmbData, file = "tests/testthat/co2RawAmbData.Rdata")

# GET a few more datasets from the calibrated data file:
# 4) get the table of Bowling gain and offset parameters.
ciso_cal <- rhdf5::h5read(master_cal_file, "/WREF/dp01/data/isoCo2")

co2B03CalTable <- ciso_cal$calData$calGainsOffsets
save(co2B03CalTable, file = "tests/testthat/co2B03CalTable.Rdata")

# 5) Bowling method calibrated reference values.
co2B03CalRefData <- ciso_cal[c("co2High_09m","co2Med_09m","co2Low_09m")]
save(co2B03CalRefData, file = "tests/testthat/co2B03CalRefData.Rdata")

# 6) Bowling method calibrated data file.
co2B03CalAmbData <- ciso_cal$`000_010_09m`
save(co2B03CalAmbData, file = "tests/testthat/co2B03CalAmbData.Rdata")

rm(list = ls(pattern = "^co2"))
rm(ciso, ucrt, ciso_cal)

#---------------------------------------
# extract h2o data for unit testing:
#---------------------------------------

wiso <- rhdf5::h5read(master_file, "/WREF/dp01/data/isoH2o")
ucrt <- rhdf5::h5read(master_file, "/WREF/dp01/ucrt/isoH2o")

# 1) get raw calibration data.
h2oRawRefData <- wiso[c("h2oHigh_03m","h2oMed_03m","h2oLow_03m")]
save(h2oRawRefData, file = "tests/testthat/h2oRawRefData.Rdata")

# 1b) get raw calibration ucrt.
h2oRawRefUcrt <- ucrt[c("h2oHigh_03m","h2oMed_03m","h2oLow_03m")]
save(h2oRawRefUcrt, file = "tests/testthat/h2oRawRefUcrt.Rdata")

# 1c) get raw calibration data using stackEddy for by_site method:
h2oRawRefDataBySite <- neonUtilities::stackEddy(master_file, level = "dp01", avg = 3)[[1]]
save(h2oRawRefDataBySite, file = "tests/testthat/h2oRawRefDataBySite.Rdata")

# 2) get calibration data after filtering.
#h2oFiltRefData <- extract_carbon_calibration_data(ciso, NULL, "high")
#save(h2oFiltRefData, file = "tests/testthat/h2oFiltRefData.Rdata")

# 3) get raw ambient data - currently from a monthly file, might be better to get from daily file.
h2oRawAmbData <- wiso$`000_010_09m`
save(h2oRawAmbData, file = "tests/testthat/h2oRawAmbData.Rdata")
