# test-data_regression

#----------------------
# CARBON FUNCTIONS
#----------------------

# test to make sure that the I/O structures across regression functions are correct.
fin <- system.file('extdata',
                   'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5',
                   package = 'NEONiso', mustWork = TRUE)
co2test <- ingest_data(fin, analyte = 'Co2')
co2data <- NEONiso:::extract_carbon_calibration_data(co2test$refe_stacked)
calDf_B03 <- NEONiso:::fit_carbon_regression(co2data, method = "Bowling_2003", calibration_half_width = 2)
calDf_LR <- NEONiso:::fit_carbon_regression(co2data, method = "linreg", calibration_half_width = 2)

test_that("fit_carbon_regression returns data.frame", {
  expect_s3_class(fit_carbon_regression(co2data, method = "Bowling_2003", calibration_half_width = 2), "data.frame")
  expect_s3_class(fit_carbon_regression(co2data, method = "linreg", calibration_half_width = 2), "data.frame")
})

test_that("calibration data frames have 8 columns", {
  expect_equal(ncol(calDf_B03), 8)
  expect_equal(ncol(calDf_LR), 8)
})


# work through ambient calibrations

# test carbon - bowling ambient calibration
temp <- calibrate_ambient_carbon_Bowling2003(co2test$ambient$`000_010_09m`,
                                             calDf_B03,
                                             site = 'ONAQ')
temp_gf <- calibrate_ambient_carbon_Bowling2003(co2test$ambient$`000_010_09m`,
                                           calDf_B03,
                                           site = 'ONAQ',
                                           gap_fill_parameters = TRUE)

test_that("calibrate_ambient_carbon_Bowling2003 returns a list",{
  expect_equal(class(temp), "list")
  expect_equal(class(temp_gf), "list")
})

test_that("calibrate_ambient_carbon_Bowling2003 returns correct variables", {
  vars <- c("dlta13CCo2", "pres", "presEnvHut", "rhEnvHut",
            "rtioMoleDry12CCo2", "rtioMoleDry13CCo2", "rtioMoleDryCo2", "rtioMoleDryH2o",
            "rtioMoleWet12CCo2", "rtioMoleWet13CCo2", "rtioMoleWetCo2", "rtioMoleWetH2o",
            "rtioMoleWetH2oEnvHut", "temp", "tempEnvHut")
  expect_equal(names(temp), vars)
  expect_equal(names(temp_gf), vars)
})

test_that("calibrated d13C values have been added to calibrate_ambient_cabron_Bowling2003 output", {
  expect_gt(ncol(temp$dlta13CCo2), ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
  expect_gt(ncol(temp_gf$dlta13CCo2), ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
})

# test carbon - linreg ambient calibration
temp <- calibrate_ambient_carbon_linreg(co2test$ambient$`000_010_09m`,
                                             calDf_LR,
                                             site = 'ONAQ')
temp_gf <- calibrate_ambient_carbon_linreg(co2test$ambient$`000_010_09m`,
                                calDf_LR,
                                site = 'ONAQ',
                                gap_fill_parameters = TRUE)

test_that("calibrate_ambient_carbon_linreg returns a list",{
  expect_equal(class(temp), "list")
  expect_equal(class(temp_gf), "list")
})

test_that("calibrate_ambient_carbon_linreg returns correct variables", {
  vars <- c("dlta13CCo2", "pres", "presEnvHut", "rhEnvHut",
            "rtioMoleDry12CCo2", "rtioMoleDry13CCo2", "rtioMoleDryCo2", "rtioMoleDryH2o",
            "rtioMoleWet12CCo2", "rtioMoleWet13CCo2", "rtioMoleWetCo2", "rtioMoleWetH2o",
            "rtioMoleWetH2oEnvHut", "temp", "tempEnvHut")
  expect_equal(names(temp), vars)
  expect_equal(names(temp_gf), vars)
})

test_that("calibrated d13C values have been added to calibrate_ambient_cabron_linreg output", {
  expect_gt(ncol(temp$dlta13CCo2), ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
  expect_gt(ncol(temp_gf$dlta13CCo2), ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
})


#----------------------
# WATER FUNCTIONS
#----------------------


