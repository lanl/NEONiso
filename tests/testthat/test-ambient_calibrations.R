# test-ambient_calibrations

load("co2RawAmbData.Rdata")
load("co2B03CalAmbData.Rdata")

test_that("calibrate_ambient_carbon_Bowling2003 errors out when bad output file name is returned.", {
  expect_error(calibrate_ambient_carbon_Bowling2003(amb_co2_data, carbon_GO_cals, "dlta13CCo2", "ONAQ", "test.h5"))
})