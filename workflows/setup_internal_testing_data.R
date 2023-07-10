# setup_testing_data.R
# rich fiorella

rm(list=ls())

library(rhdf5)
# a script to setup data to be bundled with the NEONiso package.
# exists in rbuildignore, so should not be bundled with package.
# requires install and restart w/ NEONiso.

master_file <- "NEON/DP4_00200_001/ONAQ/NEON.D15.ONAQ.DP4.00200.001.nsae.2019-06.basic.h5"
master_ccalB03_file <- "210303_CisoEC/ONAQ/NEON.D15.ONAQ.DP4.00200.001.nsae.2019-06.basic.20201020T212232Z.calibrated.h5"
master_ccalLR_file <- "210303_CisoLR/ONAQ/NEON.D15.ONAQ.DP4.00200.001.nsae.2019-06.basic.20201020T212232Z.calibrated.h5"

#---------------------------------------
# extract co2 data for unit testing:
#---------------------------------------
ciso <- rhdf5::h5read(master_file, "/ONAQ/dp01/data/isoCo2")
ucrt <- rhdf5::h5read(master_file, "/ONAQ/dp01/ucrt/isoCo2")

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
#---------------------
# Bowling method
#---------------------
# 4) get the table of Bowling gain and offset parameters.
ciso_cal <- rhdf5::h5read(master_ccalB03_file, "/ONAQ/dp01/data/isoCo2")

co2B03CalTable <- ciso_cal$calData$calGainsOffsets
save(co2B03CalTable, file = "tests/testthat/co2B03CalTable.Rdata")

# 5) Bowling method calibrated reference values.
co2B03CalRefData <- ciso_cal[c("co2High_09m","co2Med_09m","co2Low_09m")]
save(co2B03CalRefData, file = "tests/testthat/co2B03CalRefData.Rdata")

# 6) Bowling method calibrated data file.
co2B03CalAmbData <- ciso_cal$`000_010_09m`
save(co2B03CalAmbData, file = "tests/testthat/co2B03CalAmbData.Rdata")

#---------------------
# Linear regression method
#---------------------
# 4) get the table of Bowling gain and offset parameters.
ciso_cal <- rhdf5::h5read(master_ccalLR_file, "/ONAQ/dp01/data/isoCo2")

co2LRCalTable <- ciso_cal$calData$calRegressions
save(co2LRCalTable, file = "tests/testthat/co2LRCalTable.Rdata")

# 5) Bowling method calibrated reference values.
co2LRCalRefData <- ciso_cal[c("co2High_09m","co2Med_09m","co2Low_09m")]
save(co2LRCalRefData, file = "tests/testthat/co2LRCalRefData.Rdata")

# 6) Bowling method calibrated data file.
co2LRCalAmbData <- ciso_cal$`000_010_09m`
save(co2LRCalAmbData, file = "tests/testthat/co2LRCalAmbData.Rdata")

rm(list = ls(pattern = "^co2"))
rm(ciso, ucrt, ciso_cal)

#---------------------------------------
# extract h2o data for unit testing:
#---------------------------------------

wiso <- rhdf5::h5read(master_file, "/ONAQ/dp01/data/isoH2o")
ucrt <- rhdf5::h5read(master_file, "/ONAQ/dp01/ucrt/isoH2o")

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

#_---------------------
# if need to generate a new hdf5 sample file...
fid <- H5Fopen("inst/extdata/NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.h5")
# delete higher level data products
h5delete(fid, '/ONAQ/dp04/')
h5delete(fid, '/ONAQ/dp03/')
h5delete(fid, '/ONAQ/dp02/')
h5delete(fid, '/ONAQ/dp0p/')
# trim a bunch of unnecessary products.
h5delete(fid, '/ONAQ/dp01/qfqm/')
h5delete(fid, '/ONAQ/dp01/ucrt/')
h5delete(fid, '/ONAQ/dp01/data/co2Stor/')
h5delete(fid, '/ONAQ/dp01/data/h2oStor/')
h5delete(fid, '/ONAQ/dp01/ucrt/h2oStor/')
h5delete(fid, '/ONAQ/dp01/ucrt/co2Stor/')
h5delete(fid, '/ONAQ/dp01/ucrt/co2Turb/')
h5delete(fid, '/ONAQ/dp01/data/co2Turb/')
h5delete(fid, '/ONAQ/dp01/ucrt/h2oTurb/')
h5delete(fid, '/ONAQ/dp01/data/h2oTurb/')
h5delete(fid, '/ONAQ/dp01/data/soni/')
h5delete(fid, '/ONAQ/dp01/data/amrs/')
h5delete(fid, '/ONAQ/dp01/ucrt/amrs/')
h5delete(fid, '/ONAQ/dp01/ucrt/soni/')
h5delete(fid, '/ONAQ/dp01/ucrt/tempAirLvl/')
h5delete(fid, '/ONAQ/dp01/data/tempAirLvl/')
h5delete(fid, '/ONAQ/dp01/data/radiNet/')
h5delete(fid, '/ONAQ/dp01/ucrt/radiNet/')
h5delete(fid, '/ONAQ/dp01/ucrt/tempSoil/')
h5delete(fid, '/ONAQ/dp01/data/tempSoil/')
h5delete(fid, '/ONAQ/dp01/data/h2oSoilVol/')
h5delete(fid, '/ONAQ/dp01/ucrt/h2oSoilVol/')
h5delete(fid, '/ONAQ/dp01/ucrt/fluxHeatSoil/')
h5delete(fid, '/ONAQ/dp01/data/fluxHeatSoil/')
h5delete(fid, '/ONAQ/dp01/data/presBaro/')
h5delete(fid, '/ONAQ/dp01/ucrt/presBaro/')
h5delete(fid, '/ONAQ/dp01/ucrt/tempAirTop/')
h5delete(fid, '/ONAQ/dp01/data/tempAirTop/')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Arch_09m')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Arch_09m')
# start to trim away data in isotope products that aren't necessary
h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2High_30m')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Med_30m')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Low_30m')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Arch_30m')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oHigh_30m')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oMed_30m')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oLow_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2High_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Med_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Low_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Arch_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oHigh_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oMed_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oLow_30m')
# remove subproducts of isoH2o and isoCo2
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_010_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_020_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_030_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_040_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_010_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_020_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_030_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_040_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oLow_03m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oMed_03m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oHigh_03m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_010_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_020_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_030_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_040_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oLow_03m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oMed_03m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oHigh_03m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_010_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_020_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_030_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_040_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oLow_03m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oMed_03m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oHigh_03m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_010_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_020_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_030_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_040_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oLow_03m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oMed_03m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oHigh_03m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_010_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_020_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_030_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_040_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oLow_03m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oMed_03m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/h2oHigh_03m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_010_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_020_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_30m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_010_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_020_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Low_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Med_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2High_09m/pres')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_010_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_020_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Low_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Med_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2High_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_010_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_020_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Low_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Med_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2High_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_010_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_020_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Low_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Med_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2High_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_010_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_020_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Low_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Med_09m/temp')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2High_09m/temp')

h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_010_30m')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_020_30m')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_030_30m')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_040_30m')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_010_09m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_020_09m/pres')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_030_09m/pres')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_040_09m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oLow_03m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oMed_03m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oHigh_03m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_010_09m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_020_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_030_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_040_09m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oLow_03m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oMed_03m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oHigh_03m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_010_09m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_020_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_030_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_040_09m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oLow_03m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oMed_03m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oHigh_03m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_010_09m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_020_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_030_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_040_09m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oLow_03m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oMed_03m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oHigh_03m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_010_09m/temp')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_020_09m/temp')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_030_09m/temp')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_040_09m/temp')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oLow_03m/temp')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oMed_03m/temp')
# h5delete(fid, '/ONAQ/dp01/data/isoH2o/h2oHigh_03m/temp')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_010_30m')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_020_30m')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_30m')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_30m')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_010_09m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_020_09m/pres')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_09m/pres')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_09m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Low_09m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Med_09m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2High_09m/pres')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_010_09m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_020_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_09m/presEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_09m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Low_09m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Med_09m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2High_09m/presEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_010_09m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_020_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_09m/rhEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_09m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Low_09m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Med_09m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2High_09m/rhEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_010_09m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_020_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_09m/tempEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_09m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Low_09m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Med_09m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2High_09m/tempEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_010_09m/temp')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_020_09m/temp')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_09m/temp')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_09m/temp')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Low_09m/temp')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Med_09m/temp')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2High_09m/temp')
# 
# # eh just keep one level for simplicity.
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_09m')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_09m')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_030_09m')
h5delete(fid, '/ONAQ/dp01/data/isoH2o/000_040_09m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_010_09m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_020_09m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_09m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_09m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_010_09m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_020_09m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_030_09m')
h5delete(fid, '/ONAQ/dp01/ucrt/isoH2o/000_040_09m')
# 
# # keep going...
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_09m/rtioMoleWetH2oEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_09m/rtioMoleWetH2oEnvHut')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_09m/rtioMoleWet12CCo2')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_09m/rtioMoleWet12CCo2')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_030_09m/rtioMoleWet13CCo2')
h5delete(fid, '/ONAQ/dp01/data/isoCo2/000_040_09m/rtioMoleWet13CCo2')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Low_09m/rtioMoleWetH2oEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Low_09m/rtioMoleWet12CCo2')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Low_09m/rtioMoleWet13CCo2')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Med_09m/rtioMoleWetH2oEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Med_09m/rtioMoleWet12CCo2')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2Med_09m/rtioMoleWet13CCo2')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2High_09m/rtioMoleWetH2oEnvHut')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2High_09m/rtioMoleWet12CCo2')
# h5delete(fid, '/ONAQ/dp01/data/isoCo2/co2High_09m/rtioMoleWet13CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_09m/rtioMoleWetH2oEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_09m/rtioMoleWetH2oEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_09m/rtioMoleWet12CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_09m/rtioMoleWet12CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_030_09m/rtioMoleWet13CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/000_040_09m/rtioMoleWet13CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Low_09m/rtioMoleWetH2oEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Low_09m/rtioMoleWet12CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Low_09m/rtioMoleWet13CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Med_09m/rtioMoleWetH2oEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Med_09m/rtioMoleWet12CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2Med_09m/rtioMoleWet13CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2High_09m/rtioMoleWetH2oEnvHut')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2High_09m/rtioMoleWet12CCo2')
h5delete(fid, '/ONAQ/dp01/ucrt/isoCo2/co2High_09m/rtioMoleWet13CCo2')
# cut out more:


H5Fclose(fid)

#===============================================================================
#===============================================================================
#===============================================================================
# SECOND DATA FILE - STRIP 2020-11 YELL FILE TO JUST ONE HEIGHT AND STANDARDS

system("cp ~/DP4_00200_001/YELL/NEON.D12.YELL.DP4.00200.001.nsae.2020-11.basic.20210209T161116Z.h5 inst/extdata/")

# if need to generate a new hdf5 sample file...
fid <- H5Fopen("inst/extdata/NEON.D12.YELL.DP4.00200.001.nsae.2020-11.basic.20210209T161116Z.h5")
# delete higher level data products
h5delete(fid, '/YELL/dp04/')
h5delete(fid, '/YELL/dp03/')
h5delete(fid, '/YELL/dp02/')
h5delete(fid, '/YELL/dp0p/')
# trim a bunch of unnecessary products.
h5delete(fid, '/YELL/dp01/qfqm/')
h5delete(fid, '/YELL/dp01/ucrt/')
h5delete(fid, '/YELL/dp01/data/isoH2o/')
h5delete(fid, '/YELL/dp01/data/amrs/')
h5delete(fid, '/YELL/dp01/data/co2Stor/')
h5delete(fid, '/YELL/dp01/data/co2Turb/')
h5delete(fid, '/YELL/dp01/data/fluxHeatSoil/')
h5delete(fid, '/YELL/dp01/data/h2oSoilVol/')
h5delete(fid, '/YELL/dp01/data/h2oStor/')
h5delete(fid, '/YELL/dp01/data/h2oTurb/')
h5delete(fid, '/YELL/dp01/data/presBaro/')
h5delete(fid, '/YELL/dp01/data/radiNet/')
h5delete(fid, '/YELL/dp01/data/soni/')
h5delete(fid, '/YELL/dp01/data/tempAirLvl/')
h5delete(fid, '/YELL/dp01/data/tempSoil/')
h5delete(fid, '/YELL/dp01/data/tempAirTop/')

h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_30m')
h5delete(fid, '/YELL/dp01/data/isoCo2/000_020_30m')
h5delete(fid, '/YELL/dp01/data/isoCo2/000_030_30m')
h5delete(fid, '/YELL/dp01/data/isoCo2/000_040_30m')
h5delete(fid, '/YELL/dp01/data/isoCo2/000_050_30m')
h5delete(fid, '/YELL/dp01/data/isoCo2/000_020_09m')
h5delete(fid, '/YELL/dp01/data/isoCo2/000_030_09m')
h5delete(fid, '/YELL/dp01/data/isoCo2/000_040_09m')
h5delete(fid, '/YELL/dp01/data/isoCo2/000_050_09m')
h5delete(fid, '/YELL/dp01/data/isoCo2/co2Arch_09m')
h5delete(fid, '/YELL/dp01/data/isoCo2/co2Arch_30m')
h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_30m')
h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_30m')
h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_30m')
# 
# h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/rtioMoleWet12CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/rtioMoleWet13CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/rtioMoleDry12CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/rtioMoleDry13CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/rtioMoleWetCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/rtioMoleWetH2oEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/rhEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/presEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/tempEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/rtioMoleDryH2o')
# h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/rtioMoleWetH2o')
# h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/temp')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/000_010_09m/pres')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/rtioMoleWet12CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/rtioMoleWet13CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/rtioMoleWetCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/rtioMoleWetH2oEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/rhEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/presEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/tempEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/rtioMoleDryH2o')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/rtioMoleWetH2o')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/temp')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2High_09m/pres')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/rtioMoleWet12CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/rtioMoleWet13CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/rtioMoleWetCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/rtioMoleWetH2oEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/rhEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/presEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/tempEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/rtioMoleDryH2o')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/rtioMoleWetH2o')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/temp')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Med_09m/pres')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/rtioMoleWet12CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/rtioMoleWet13CCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/rtioMoleWetCo2')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/rtioMoleWetH2oEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/rhEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/presEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/tempEnvHut')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/rtioMoleDryH2o')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/rtioMoleWetH2o')
# h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/temp')
# #h5delete(fid, '/YELL/dp01/data/isoCo2/co2Low_09m/pres')

H5Fclose(fid)

system('~/opt/anaconda3/bin/h5repack inst/extdata/NEON.D12.YELL.DP4.00200.001.nsae.2020-11.basic.20210209T161116Z.h5 inst/extdata/NEON.D12.YELL.DP4.00200.001.nsae.2020-11.basic.packed.h5')

