# test-data_extraction

test_that("extract_water_calibration_data throws errors when it should",{
  expect_error(extract_water_calibration_data("temp","temp","low",ucrt_source = "ucrt", method = "by_site"))
  expect_error(extract_water_calibration_data("temp","temp","low",ucrt_source = "ucrt", method = "by_month"))
  expect_error(extract_water_calibration_data("temp","temp","spork",ucrt_source = "data", method = "by_site"))
  expect_error(extract_water_calibration_data("temp","temp","spork",ucrt_source = "data", method = "by_month"))
})