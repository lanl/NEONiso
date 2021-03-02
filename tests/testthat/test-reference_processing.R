# test-reference_processing

#----------------------------
# test carbon function

load("co2RawRefData.Rdata")
load("co2RawRefUcrt.Rdata")

test_that("extract_carbon_calibration_data throws errors when it should",{
  expect_error(extract_carbon_calibration_data(co2RawRefData, co2RawRefData, "spork", ucrt_source = "data"))
  expect_error(extract_carbon_calibration_data(co2RawRefData, co2RawRefUcrt, "spork", ucrt_source = "ucrt"))
})

test_that("carbon data frames all end up with 15 columns after extract_carbon_calibration_data", {
  expect_equal(ncol(extract_carbon_calibration_data(co2RawRefData, co2RawRefData, "low", ucrt_source = "data")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(co2RawRefData, co2RawRefData, "med", ucrt_source = "data")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(co2RawRefData, co2RawRefData, "high", ucrt_source = "data")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(co2RawRefData, co2RawRefUcrt, "low", ucrt_source = "ucrt")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(co2RawRefData, co2RawRefUcrt, "med", ucrt_source = "ucrt")), 15)
  expect_equal(ncol(extract_carbon_calibration_data(co2RawRefData, co2RawRefUcrt, "high", ucrt_source = "ucrt")), 15)
})

#test_that("all three standards work correctly in extract_carbon_calibration_data", {
#expect_
#})


#--------------------
# test water function

load("h2oRawRefData.Rdata")
load("h2oRawRefUcrt.Rdata")
load("h2oRawRefDataBySite.Rdata")

test_that("extract_water_calibration_data throws errors when it should",{
  expect_error(extract_water_calibration_data(h2oRawRefData,h2oRawRefUcrt,"low",ucrt_source = "ucrt", method = "by_site"))
  expect_error(extract_water_calibration_data(h2oRawRefData,h2oRawRefUcrt,"low",ucrt_source = "ucrt", method = "by_month")) # passes, but doesn't hit the right line?
  expect_error(extract_water_calibration_data(h2oRawRefData,h2oRawRefData,"spork",ucrt_source = "data", method = "by_site"))
  expect_error(extract_water_calibration_data(h2oRawRefData,h2oRawRefData,"spork",ucrt_source = "data", method = "by_month"))
  expect_error(extract_water_calibration_data(h2oRawRefData,h2oRawRefData,"spork",ucrt_source = "data", method = "by_cheese"))
})

test_that("water data frames have correct number of columns after extract_carbon_calibration_data", {
  expect_equal(ncol(extract_water_calibration_data(h2oRawRefDataBySite,NULL,"low",ucrt_source = "data", method = "by_site")),16) # change back to 15 after kludge fix
  expect_equal(ncol(extract_water_calibration_data(h2oRawRefDataBySite,NULL,"med",ucrt_source = "data", method = "by_site")),16) # in calibrate_water_linreg function.
  expect_equal(ncol(extract_water_calibration_data(h2oRawRefDataBySite,NULL,"high",ucrt_source = "data", method = "by_site")),16)
# these tests need to be redesigned.
#  expect_equal(ncol(extract_water_calibration_data(h2oRawRefData,NULL,"low",ucrt_source = "data", method = "by_month")),21)
#  expect_equal(ncol(extract_water_calibration_data(h2oRawRefData,NULL,"med",ucrt_source = "data", method = "by_month")),21)
#  expect_equal(ncol(extract_water_calibration_data(h2oRawRefData,NULL,"high",ucrt_source = "data", method = "by_month")),21)
})

# test functions using the output of extract_water_calibration_data(method = "by_month")


# run raw reference data rhrough the extract data frame.
low_co2 <- extract_carbon_calibration_data(co2RawRefData, co2RawRefData, "low", ucrt_source = "data")
low_co2_filt <- select_daily_reference_data(low_co2, analyte = "co2")
low_h2o <- extract_water_calibration_data(h2oRawRefData$h2oLow_03m, h2oRawRefUcrt, "low", ucrt_source = "data", method = "by_month")
low_h2o_filt <- select_daily_reference_data(low_h2o, analyte = "h2o")

test_that("select_daily_reference_data errors if invalid analyte given", {
  expect_error(select_daily_reference_data(low_co2, analyte = "ch4"))
  expect_error(select_daily_reference_data(low_h2o, analyte = "ch4"))
})

test_that("select_daily_reference_data errors if standard_df and analyte don't match", {
  expect_error(select_daily_reference_data(low_co2, analyte = "h2o"))
  expect_error(select_daily_reference_data(low_h2o, analyte = "co2"))
})

test_that("select_daily_reference_data returns data frame if valid analyte given", {
  expect_true(is.data.frame(low_co2_filt))
  expect_true(is.data.frame(low_h2o_filt))
})

