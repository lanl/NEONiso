# test-data_extraction

#----------------------------
# test carbon function

load("ref_test_data.Rdata")
load("ref_test_ucrt.Rdata")

test_that("extract_carbon_calibration_data throws errors when it should",{
  expect_error(extract_carbon_calibration_data(ref_test_data, ref_test_data, "spork", ucrt_source = "data"))
  expect_error(extract_carbon_calibration_data(ref_test_data, ref_test_ucrt, "spork", ucrt_source = "ucrt"))
})

test_that("carbon data frames all end up with 15 columns after extract_carbon_calibration_data", {
  expect_equal(ncol(extract_carbon_calibration_data(ref_test_data, ref_test_data, "low", ucrt_source = "data")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(ref_test_data, ref_test_data, "med", ucrt_source = "data")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(ref_test_data, ref_test_data, "high", ucrt_source = "data")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(ref_test_data, ref_test_ucrt, "low", ucrt_source = "ucrt")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(ref_test_data, ref_test_ucrt, "med", ucrt_source = "ucrt")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(ref_test_data, ref_test_ucrt, "high", ucrt_source = "ucrt")), 15)
})

#test_that("all three standards work correctly in extract_carbon_calibration_data", {
#expect_
#})


#--------------------
# test water function

load("ref_wat_data.Rdata")
load("ref_wat_ucrt.Rdata")
load("ref_wat_dat_bySite.Rdata")

test_that("extract_water_calibration_data throws errors when it should",{
  expect_error(extract_water_calibration_data(ref_wat_data,ref_wat_ucrt,"low",ucrt_source = "ucrt", method = "by_site"))
  expect_error(extract_water_calibration_data(ref_wat_data,ref_wat_ucrt,"low",ucrt_source = "ucrt", method = "by_month")) # passes, but doesn't hit the right line?
  expect_error(extract_water_calibration_data(ref_wat_data,ref_wat_data,"spork",ucrt_source = "data", method = "by_site"))
  expect_error(extract_water_calibration_data(ref_wat_data,ref_wat_data,"spork",ucrt_source = "data", method = "by_month"))
  expect_error(extract_water_calibration_data(ref_wat_data,ref_wat_data,"spork",ucrt_source = "data", method = "by_cheese"))
})

test_that("water data frames have correct number of columns after extract_carbon_calibration_data", {
  expect_equal(ncol(extract_water_calibration_data(ref_wat_dat_bySite,NULL,"low",ucrt_source = "data", method = "by_site")),16) # change back to 15 after kludge fix 
  expect_equal(ncol(extract_water_calibration_data(ref_wat_dat_bySite,NULL,"med",ucrt_source = "data", method = "by_site")),16) # in calibrate_water_linreg function.
  expect_equal(ncol(extract_water_calibration_data(ref_wat_dat_bySite,NULL,"high",ucrt_source = "data", method = "by_site")),16)
  # these tests need to be redesigned.
#  expect_equal(ncol(extract_water_calibration_data(ref_wat_data,NULL,"low",ucrt_source = "data", method = "by_month")),21)
#  expect_equal(ncol(extract_water_calibration_data(ref_wat_data,NULL,"med",ucrt_source = "data", method = "by_month")),21)
#  expect_equal(ncol(extract_water_calibration_data(ref_wat_data,NULL,"high",ucrt_source = "data", method = "by_month")),21)
})

# test functions using the output of extract_water_calibration_data(method = "by_month")

