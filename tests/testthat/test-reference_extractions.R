# test-reference_extractions.R

fin <- system.file('extdata',
                   'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.20201020T211037Z.packed.h5',
                   package = 'NEONiso', mustWork = TRUE)
ciso <- neonUtilities::stackEddy(fin, level = 'dp01', avg = 9)

test_that("extract_carbon_calibration_data returns a data frame", {
  expect_true(is.data.frame(extract_carbon_calibration_data(ciso)))
})

test_that("extract_carbon_calibration_data returns 23 columns", {
 expect_equal(ncol(extract_carbon_calibration_data(ciso)),23)
})

# test standard correction
refe <- extract_carbon_calibration_data(ciso)
test_that("correct_carbon_ref_cval returns 23 columns", {
  expect_equal(ncol(correct_carbon_ref_cval(refe, 'ONAQ')),23)
})

#-------------------------------------------------------------------------------
# test water functions - this will need to be changed to match carbon structure.

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
})


