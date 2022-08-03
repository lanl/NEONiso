# test-data_regression

#----------------------
# CARBON FUNCTIONS
#----------------------

# test to make sure that the I/O structures across regression functions are correct.
fin <- system.file('extdata',
                   'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5',
                   package = 'NEONiso', mustWork = TRUE)
co2test <- ingest_data(fin, analyte = 'Co2')
co2data <- NEONiso:::extract_carbon_calibration_data(co2test$refe_stacked)
calDf_B03 <- NEONiso:::fit_carbon_regression(co2data, method = "Bowling_2003", calibration_half_width = 2)
calDf_LR <- NEONiso:::fit_carbon_regression(co2data, method = "linreg", calibration_half_width = 2)

test_that("fit_carbon_regression returns data.frame", {
  skip_on_cran()
  expect_s3_class(fit_carbon_regression(co2data, method = "Bowling_2003", calibration_half_width = 2), "data.frame")
  expect_s3_class(fit_carbon_regression(co2data, method = "linreg", calibration_half_width = 2), "data.frame")
})

test_that("calibration data frames have 14 columns", {
  skip_on_cran()
  expect_equal(ncol(calDf_B03), 14)
  expect_equal(ncol(calDf_LR), 14)
})


# work through ambient calibrations

# test carbon - bowling ambient calibration
temp <- calibrate_ambient_carbon_Bowling2003(co2test$ambient$`000_010_09m`,
                                             calDf_B03,
                                             site = 'ONAQ')
temp_gf <- calibrate_ambient_carbon_Bowling2003(co2test$ambient$`000_010_09m`,
                                           calDf_B03,
                                           site = 'ONAQ',
                                           gap_fill_parameters = TRUE)

test_that("calibrate_ambient_carbon_Bowling2003 returns a list",{
  skip_on_cran()
  expect_equal(class(temp), "list")
  expect_equal(class(temp_gf), "list")
})

test_that("calibrate_ambient_carbon_Bowling2003 returns correct variables", {
  skip_on_cran()
  vars <- c("dlta13CCo2", "pres", "presEnvHut", "rhEnvHut",
            "rtioMoleDry12CCo2", "rtioMoleDry13CCo2", "rtioMoleDryCo2", "rtioMoleDryH2o",
            "rtioMoleWet12CCo2", "rtioMoleWet13CCo2", "rtioMoleWetCo2", "rtioMoleWetH2o",
            "rtioMoleWetH2oEnvHut", "temp", "tempEnvHut")
  expect_equal(names(temp), vars)
  expect_equal(names(temp_gf), vars)
})

test_that("calibrated d13C values have been added to calibrate_ambient_cabron_Bowling2003 output", {
  skip_on_cran()
  expect_gt(ncol(temp$dlta13CCo2), ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
  expect_gt(ncol(temp_gf$dlta13CCo2), ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
})

# test carbon - linreg ambient calibration
temp <- calibrate_ambient_carbon_linreg(co2test$ambient$`000_010_09m`,
                                             calDf_LR,
                                             site = 'ONAQ')
temp_gf <- calibrate_ambient_carbon_linreg(co2test$ambient$`000_010_09m`,
                                calDf_LR,
                                site = 'ONAQ',
                                gap_fill_parameters = TRUE)

test_that("calibrate_ambient_carbon_linreg returns a list",{
  skip_on_cran()
  expect_equal(class(temp), "list")
  expect_equal(class(temp_gf), "list")
})

test_that("calibrate_ambient_carbon_linreg returns correct variables", {
  skip_on_cran()
  vars <- c("dlta13CCo2", "pres", "presEnvHut", "rhEnvHut",
            "rtioMoleDry12CCo2", "rtioMoleDry13CCo2", "rtioMoleDryCo2", "rtioMoleDryH2o",
            "rtioMoleWet12CCo2", "rtioMoleWet13CCo2", "rtioMoleWetCo2", "rtioMoleWetH2o",
            "rtioMoleWetH2oEnvHut", "temp", "tempEnvHut")
  expect_equal(names(temp), vars)
  expect_equal(names(temp_gf), vars)
})

test_that("calibrated d13C values have been added to calibrate_ambient_cabron_linreg output", {
  skip_on_cran()
  expect_gt(ncol(temp$dlta13CCo2), ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
  expect_gt(ncol(temp_gf$dlta13CCo2), ncol(co2test$ambient$`000_010_09m`$dlta13CCo2))
})


#----------------------
# WATER FUNCTIONS
#----------------------

# stack data available for a given site into a single timeseries.
wiso_ref <- neonUtilities::stackEddy(fin, level = "dp01", avg = 3)

# extract standards data.
high <- subset(wiso_ref[["ONAQ"]], wiso_ref[["ONAQ"]]$verticalPosition == 'h2oHigh')
med  <- subset(wiso_ref[["ONAQ"]], wiso_ref[["ONAQ"]]$verticalPosition == 'h2oMed')
low  <- subset(wiso_ref[["ONAQ"]], wiso_ref[["ONAQ"]]$verticalPosition == 'h2oLow')

# restructure standards data.  
high_rs <- extract_water_calibration_data(high, standard = 'high', method = 'by_site')
med_rs  <- extract_water_calibration_data(med, standard = 'med', method = 'by_site')
low_rs  <- extract_water_calibration_data(low, standard = 'low', method = 'by_site')

# add fix for NEON standard swap.
low_rs <- swap_standard_isotoperatios(low_rs)
med_rs <- swap_standard_isotoperatios(med_rs)
high_rs <- swap_standard_isotoperatios(high_rs)

# convert times in these data.frames (btime and etime) to posixct
low_rs$btime <- convert_NEONhdf5_to_POSIXct_time(low_rs$btime)
low_rs$etime <- convert_NEONhdf5_to_POSIXct_time(low_rs$etime)
med_rs$btime <- convert_NEONhdf5_to_POSIXct_time(med_rs$btime)
med_rs$etime <- convert_NEONhdf5_to_POSIXct_time(med_rs$etime)
high_rs$btime <- convert_NEONhdf5_to_POSIXct_time(high_rs$btime)
high_rs$etime <- convert_NEONhdf5_to_POSIXct_time(high_rs$etime)

#--------------------------------------------------------------
# Ensure same number of measurements for each standard
#--------------------------------------------------------------
# add group ids using run length encoding based on time differences.
thres_hours <- as.difftime("04:00:00", # assume any time difference 
                           format = "%H:%M:%S", # > 4 hours is a new reference measurement
                           units = "mins")

high_rs <- high_rs %>%
  mutate(time_diff = ifelse(.data$btime - lag(.data$btime) > thres_hours, 1, 0))
high_rs$periods <- rleidv(high_rs, "time_diff") %/% 2

med_rs <- med_rs %>%
  mutate(time_diff = ifelse(.data$btime - lag(.data$btime) > thres_hours, 1, 0))
med_rs$periods <- rleidv(med_rs, "time_diff") %/% 2

low_rs <- low_rs %>%
  mutate(time_diff = ifelse(.data$btime - lag(.data$btime) > thres_hours, 1, 0))
low_rs$periods <- rleidv(low_rs, "time_diff") %/% 2

high_rs <- high_rs %>%
  group_by(.data$periods) %>%
  filter(.data$d18O_meas_n > 30 | is.na(.data$d18O_meas_n)) %>%
  slice_tail(n = 3) %>%
  ungroup()

med_rs <- med_rs %>%
  group_by(.data$periods) %>%
  filter(.data$d18O_meas_n > 30 | is.na(.data$d18O_meas_n)) %>%
  slice_tail(n = 3) %>%
  ungroup()

low_rs <- low_rs %>%
  group_by(.data$periods) %>%
  filter(.data$d18O_meas_n > 30 | is.na(.data$d18O_meas_n)) %>%
  slice_tail(n = 3) %>%
  ungroup()

stds <- do.call(rbind, list(high_rs, med_rs, low_rs))

test_that("fit_water_regression returns dataframe with 8 columns", {
  skip_on_cran()
  expect_equal(ncol(fit_water_regression(stds,
                                    calibration_half_width = 14,
                                    slope_tolerance = 9999,
                                    r2_thres = 0.9)), 8)
  
  expect_true(is.data.frame(fit_water_regression(stds,
                                                 calibration_half_width = 14,
                                                 slope_tolerance = 9999,
                                                 r2_thres = 0.9)))
})

#-----------------------------
# now test ambient functions:
out <- fit_water_regression(stds,
                            calibration_half_width = 14,
                            slope_tolerance = 9999,
                            r2_thres = 0.9)
data_by_height_by_var <- restructure_ambient_data(fin, 'H2o')

# test restructure_ambient_data:
test_that("restructure_ambient_data returns a list with 2 elements for modified ONAQ file", {
  expect_equal(length(restructure_ambient_data(fin, 'H2o')), 2)
  expect_type(data_by_height_by_var, "list")
})



