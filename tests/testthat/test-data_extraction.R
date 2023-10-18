# test-data_extraction

# co2 test first - testing extract_carbon_calibration_data function

fin <- system.file("extdata",
                   "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
                   package = 'NEONiso', mustWork = TRUE)
# 1) for CO2:
co2test <- ingest_data(fin, analyte = "Co2", amb_avg = 9, ref_avg = 9)

test_that("extract_carbon_calibration_data fails if incorrect list provided to function", {
  expect_error(extract_carbon_calibration_data(co2test$reference))
  expect_error(extract_carbon_calibration_data(fin))
  expect_silent(extract_carbon_calibration_data(co2test$refe_stacked))
})

test_that("extract_carbon_calibration_data output has correct structure", {
  tmp <- extract_carbon_calibration_data(co2test$refe_stacked)
  expect_equal(ncol(tmp), 23)
  expect_s3_class(tmp$timeBgn, "POSIXct")
  expect_s3_class(tmp$timeEnd, "POSIXct")
})

test_that("extract_carbon_calibration_data removes standards correctly", {
  
  expect_less_than(nrow(extract_carbon_calibration_data(co2test$refe_stacked,
                                                   standards = c("co2Low", "co2Med"))),
                   nrow(extract_carbon_calibration_data(co2test$refe_stacked)))
  
  expect_less_than(nrow(extract_carbon_calibration_data(co2test$refe_stacked,
                                                        standards = c("co2Low", "co2High"))),
                   nrow(extract_carbon_calibration_data(co2test$refe_stacked)))
  
  expect_less_than(nrow(extract_carbon_calibration_data(co2test$refe_stacked,
                                                        standards = "co2Low")),
                   nrow(extract_carbon_calibration_data(co2test$refe_stacked,
                                                        standards = c("co2High", "co2Med"))))
})

# 2) for H2O:
h2otest <- ingest_data(fin, analyte = "H2o", amb_avg = 9, ref_avg = 3)

test_that("extract_carbon_calibration_data fails if incorrect list provided to function", {
  expect_error(extract_water_calibration_data(h2otest$reference))
  expect_error(extract_water_calibration_data(fin))
  expect_silent(extract_water_calibration_data(h2otest$refe_stacked))
})

h2otest2 <- extract_water_calibration_data(h2otest$refe_stacked)


test_that("extract_water_calibration_data output has correct structure", {
  tmp <- extract_water_calibration_data(h2otest$refe_stacked)
  expect_equal(ncol(tmp), 23)
  expect_s3_class(tmp$timeBgn, "POSIXct")
  expect_s3_class(tmp$timeEnd, "POSIXct")
})


# wiso_ref <- neonUtilities::stackEddy(fin, level = "dp01", avg = 3)
# 
# high <- subset(wiso_ref[["ONAQ"]], 
#                wiso_ref[["ONAQ"]]$verticalPosition == "h2oHigh")
# med  <- subset(wiso_ref[["ONAQ"]], 
#                wiso_ref[["ONAQ"]]$verticalPosition == "h2oMed")
# low  <- subset(wiso_ref[["ONAQ"]], 
#                wiso_ref[["ONAQ"]]$verticalPosition == "h2oLow")
# 
# test_that("extract_water_calibration_data returns 16 columns for by_site method", {
#   expect_equal(ncol(extract_water_calibration_data(high,
#                                                    standard = "high",
#                                                    method = "by_site")), 16)
#   expect_equal(ncol(extract_water_calibration_data(med,
#                                                    standard = "med",
#                                                    method = "by_site")), 16)
#   expect_equal(ncol(extract_water_calibration_data(low,
#                                                    standard = "low",
#                                                    method = "by_site")), 16)
# })
# 
# test_that("extract_water_calibration_data returns error if invalid standard provided", {
#   expect_error(extract_water_calibration_data(low,
#                                               standard = "cheese",
#                                               method = "by_site"))
#   expect_error(extract_water_calibration_data(low,
#                                               standard = "cheese",
#                                               method = "by_month"))
# })
# 

