# test-data_extraction

# co2 test first - testing extract_carbon_calibration_data function

fin <- system.file('extdata',
                   'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5',
                   package = 'NEONiso', mustWork = TRUE)
# 1) for CO2:
co2test <- ingest_data(fin, analyte = 'Co2')

test_that("extract_carbon_calibration_data fails if incorrect list provided to function", {
  expect_error(extract_carbon_calibration_data(co2test$reference))
  expect_silent(extract_carbon_calibration_data(co2test$refe_stacked))
})

test_that("extract_carbon_calibration_data output has correct structure", {
  tmp <- extract_carbon_calibration_data(co2test$refe_stacked)
  expect_equal(ncol(tmp), 23)
  expect_s3_class(tmp$timeBgn, "POSIXct")
  expect_s3_class(tmp$timeEnd, "POSIXct")
})