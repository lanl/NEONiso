# test-ambient_calibrations

load("co2RawAmbData.Rdata")

#-----------------------------------
# Carbon - Bowling method
load("co2B03CalAmbData.Rdata")
load("co2B03CalTable.Rdata")

co2B03CalTablePosix <- co2B03CalTable
co2B03CalTablePosix$start <- convert_NEONhdf5_to_POSIXct_time(co2B03CalTablePosix$valid_period_start)
co2B03CalTablePosix$end   <- convert_NEONhdf5_to_POSIXct_time(co2B03CalTablePosix$valid_period_end)

test_that("calibrate_ambient_carbon_Bowling2003 errors out when bad output file name is returned.", {
  expect_error(calibrate_ambient_carbon_Bowling2003(co2RawAmbData, co2B03CalTablePosix, "dlta13CCo2",
                                                    "ONAQ", "test.h5"))
})

test_that("calibrate_ambient_carbon_Bowling2003 finishes when bad output file name is returned.", {
  expect_silent(calibrate_ambient_carbon_Bowling2003(co2RawAmbData, co2B03CalTablePosix,"dlta13CCo2",
                                                     "ONAQ", 'test.h5', write_to_file = FALSE))
})

test_that("gap filling in calibrate_ambient_carbon_Bowling2003 issues a message", {
  expect_output(calibrate_ambient_carbon_Bowling2003(co2RawAmbData, co2B03CalTablePosix,"dlta13CCo2",
                         "ONAQ", 'test.h5', write_to_file = FALSE, gap_fill_parameters = TRUE),
                "Gap filling calibrations...")
})

# change regression parameters in first and 8th row to trigger gapfilling for low r2

#-----------------------------------
# Carbon - Linear regression
load("co2LRCalAmbData.Rdata")
load("co2LRCalTable.Rdata")

co2LRCalTablePosix <- co2LRCalTable
co2LRCalTablePosix$start <- convert_NEONhdf5_to_POSIXct_time(co2LRCalTablePosix$valid_period_start)
co2LRCalTablePosix$end   <- convert_NEONhdf5_to_POSIXct_time(co2LRCalTablePosix$valid_period_end)

test_that("calibrate_ambient_carbon_linreg errors out when bad output file name is returned.", {
  expect_error(calibrate_ambient_carbon_linreg(coRawAmbData, co2LRCalTablePosix, "dlta13CCo2",
                                                    "ONAQ", "test.h5"))
})

test_that("calibrate_ambient_carbon_linreg finishes when bad output file name is returned.", {
  expect_silent(calibrate_ambient_carbon_linreg(co2RawAmbData, co2LRCalTablePosix,"dlta13CCo2",
                                                     "ONAQ", 'test.h5', write_to_file = FALSE))
})

test_that("gap filling in calibrate_ambient_carbon_linreg issues a message", {
  expect_output(calibrate_ambient_carbon_linreg(co2RawAmbData, co2LRCalTablePosix,"dlta13CCo2",
                                                     "ONAQ", 'test.h5', write_to_file = FALSE, gap_fill_parameters = TRUE),
                "Gap filling calibrations...")
})
