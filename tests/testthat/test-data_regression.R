# test-data_regression

# test to make sure that the I/O structures across regression functions are correct.
fin <- system.file('extdata',
                   'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5',
                   package = 'NEONiso', mustWork = TRUE)
co2test <- ingest_data(fin, analyte = 'Co2')
co2data <- extract_carbon_calibration_data(co2test$refe_stacked)
calDf_B03 <- fit_carbon_regression(co2data, method = "Bowling_2003", calibration_half_width = 2)

test_that("fit_carbon_regression returns data.frame", {
  expect_s3_class(fit_carbon_regression(co2data, method = "Bowling_2003", calibration_half_width = 2), "data.frame")
  expect_s3_class(fit_carbon_regression(co2data, method = "linreg", calibration_half_width = 2), "data.frame")
})