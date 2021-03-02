# test-standard_processing
# functions in process_calibration_data

load("ref_test_data.Rdata")
load("ref_wat_data.Rdata")
load("ref_wat_dat_bySite.Rdata")

test_that("select_daily_reference_data errors if invalid analyte given", {
  expect_error(select_daily_reference_data(ref_test_data, analyte = "ch4"))
  expect_error(select_daily_reference_data(ref_wat_data, analyte = "ch4"))
})

test_that("select_daily_reference_data errors if standard_df and analyte don't match", {
  expect_error(select_daily_reference_data(ref_test_data, analyte = "h2o"))
  expect_error(select_daily_reference_data(ref_wat_data, analyte = "co2"))
})

#test_that("select_daily_reference_data returns data frame if valid analyte given", {
  #expect_type(select_daily_reference_data(as.data.frame(ref_test_data$co2High_09m), analyte = "co2"), "data.frame")
  #expect_type(select_daily_reference_data(ref_wat_data$h2oHigh_03m, analyte = "h2o"), "data.frame")
#})