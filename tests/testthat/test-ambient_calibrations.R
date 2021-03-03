# test-ambient_calibrations

load("co2RawAmbData.Rdata")
load("co2B03CalAmbData.Rdata")

test_that("calibrate_ambient_carbon_Bowling2003 errors out when bad output file name is returned.", {
  expect_error(calibrate_ambient_carbon_Bowling2003(co2RawAmbData, co2B03CalAmbData, "dlta13CCo2", "ONAQ", "test.h5"))
})

#test_that("calibrate_ambient_carbon_Bowling2003 finishes when bad output file name is returned.", {
calibrate_ambient_carbon_Bowling2003(co2RawAmbData, co2B03CalAmbData,"dlta13CCo2", "ONAQ", 'test.h5', write_to_file = FALSE)
#})