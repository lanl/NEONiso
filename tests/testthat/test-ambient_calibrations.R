# test-ambient_calibrations

load("amb_co2_data.Rdata")
load("carbon_GO_cals.Rdata")

test_that("calibrate_ambient_carbon_Bowling2003 errors out when bad output file name is returned.", {
  expect_error(calibrate_ambient_carbon_Bowling2003(amb_co2_data, carbon_GO_cals, 
                                                    "dlta13CCo2", "ONAQ", "test.h5",
                                                    force_to_end = FALSE,
                                                    force_to_beginning = FALSE,
                                                    filter_data = FALSE,
                                                    r2_thres = 0.95))
})