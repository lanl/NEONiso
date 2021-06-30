# test-reference_extractions.R

# fin <- system.file('extdata',
#                    'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.20201020T211037Z.packed.h5',
#                    package = 'NEONiso', mustWork = TRUE)
# ciso <- neonUtilities::stackEddy(fin, level = 'dp01', avg = 9)

# test_that("extract_carbon_calibration_data returns a data frame", {
#   expect_true(is.data.frame(extract_carbon_calibration_data(ciso)))
# })

# test_that("extract_carbon_calibration_data returns 23 columns", {
#  expect_equal(ncol(extract_carbon_calibration_data(ciso)),23)
# })

# test standard correction
#refe <- extract_carbon_calibration_data(ciso)
# what if output has 0 rows?
#refe2 <- refe[0,] # delete all rows to test error handling.
# 
# test_that("correct_carbon_ref_cval returns 23 columns for all sites that require corrections", {
#   expect_equal(ncol(correct_carbon_ref_cval(refe, 'ONAQ')),23)
#   expect_equal(ncol(correct_carbon_ref_cval(refe, 'WOOD')),23)
#   expect_equal(ncol(correct_carbon_ref_cval(refe, 'BLAN')),23)
#   expect_equal(ncol(correct_carbon_ref_cval(refe, 'STER')),23)
#   expect_equal(ncol(correct_carbon_ref_cval(refe, 'ORNL')),23)
#   expect_equal(ncol(correct_carbon_ref_cval(refe, 'TREE')),23)
#   expect_equal(ncol(correct_carbon_ref_cval(refe, 'BARR')),23)
#   expect_equal(ncol(correct_carbon_ref_cval(refe, 'SRER')),23)
#   expect_equal(ncol(correct_carbon_ref_cval(refe, 'HARV')),23)
# })
# 
# test_that("correct_carbon_ref_cval doesn't change data frame if site doesn't need correction", {
#   expect_equal(correct_carbon_ref_cval(refe, 'HARV'),refe)
# })
# 
# # test outputs of fit_carbon_regression - should have 8 column dataframes for either method.
# test_that("fit_carbon_regression returns 8 columns (Bowling method)", {
#   expect_equal(ncol(fit_carbon_regression(refe, 'Bowling_2003', calibration_half_width = 0.5)),8)
#   expect_equal(ncol(fit_carbon_regression(refe2, 'Bowling_2003', calibration_half_width = 0.5)),8)
# })
# 
# test_that("fit_carbon_regression returns 8 columns (linreg method)", {
#   expect_equal(ncol(fit_carbon_regression(refe, 'linreg', calibration_half_width = 0.5)),8)
# 
#   expect_equal(ncol(fit_carbon_regression(refe2, 'linreg', calibration_half_width = 0.5)),8)
# })

# test calibration functions.
# ciso <- rhdf5::h5read(fin, paste0("/ONAQ/dp01/data/isoCo2"))
# cal_B03 <- fit_carbon_regression(refe, 'Bowling_2003', calibration_half_width = 0.5)
# cal_lr <- fit_carbon_regression(refe, 'linreg', calibration_half_width = 0.5)
# 
# test_that("calibrate_ambient_carbon_Bowling2003 returns a list", {
#   expect_true(is.list(calibrate_ambient_carbon_Bowling2003(
#     amb_data_list = ciso[['000_030_09m']],
#     caldf = cal_B03, site = 'ONAQ')))
# })
# 
# test_that("calibrate_ambient_carbon_linreg returns a list", {
#   expect_true(is.list(calibrate_ambient_carbon_linreg(
#     amb_data_list = ciso[['000_030_09m']],
#     caldf = cal_lr, site = 'ONAQ')))
# })
# 
# # test gap filling - there's an error lurking here
# # where if all values for a month have r2 < r2_thres,
# # code will fail. need to update in future release.
# cal_B03$r2_12C[1]  <- 0.001
# cal_lr$d13C_r2[1]  <- 0.001
# 
# test_that("ambient gap filling triggers correctly when r2 < r2_thres", {
# expect_output(calibrate_ambient_carbon_linreg(
#   amb_data_list = ciso[['000_030_09m']],
#   caldf = cal_lr, site = 'ONAQ', gap_fill_parameters = TRUE),
#    "Gap filling calibrations...")
#  expect_output(calibrate_ambient_carbon_Bowling2003(
#    amb_data_list = ciso[['000_030_09m']],
#    caldf = cal_B03, site = 'ONAQ', gap_fill_parameters = TRUE),
#    "Gap filling calibrations...")
# })
# 
# # test reference data corrections:
# low <- rhdf5::h5read(fin,"/ONAQ/dp01/data/isoCo2/co2Low_09m")
# med <- rhdf5::h5read(fin,"/ONAQ/dp01/data/isoCo2/co2Med_09m")
# hig <- rhdf5::h5read(fin,"/ONAQ/dp01/data/isoCo2/co2High_09m")
# test_that("calibrate_standards_carbon returns 23 columns for all sites that require corrections", {
#   expect_true(is.list(calibrate_standards_carbon(cal_B03, med, correct_bad_refvals = TRUE, site = 'ONAQ', refGas = 'med')))
#   expect_true(is.list(calibrate_standards_carbon(cal_B03, med, correct_bad_refvals = TRUE, site = 'WOOD', refGas = 'med')))
#   expect_true(is.list(calibrate_standards_carbon(cal_B03, med, correct_bad_refvals = TRUE, site = 'BLAN', refGas = 'med')))
#   expect_true(is.list(calibrate_standards_carbon(cal_B03, med, correct_bad_refvals = TRUE, site = 'STER', refGas = 'med')))
#   expect_true(is.list(calibrate_standards_carbon(cal_B03, med, correct_bad_refvals = TRUE, site = 'ORNL', refGas = 'med')))
#   expect_true(is.list(calibrate_standards_carbon(cal_B03, med, correct_bad_refvals = TRUE, site = 'TREE', refGas = 'med')))
#   expect_true(is.list(calibrate_standards_carbon(cal_B03, med, correct_bad_refvals = TRUE, site = 'BARR', refGas = 'med')))
#   expect_true(is.list(calibrate_standards_carbon(cal_B03, med, correct_bad_refvals = TRUE, site = 'SRER', refGas = 'med')))
#   expect_true(is.list(calibrate_standards_carbon(cal_B03, med, correct_bad_refvals = TRUE, site = 'HARV', refGas = 'med')))
#   expect_true(is.list(calibrate_standards_carbon(cal_lr, low, correct_bad_refvals = TRUE, site = 'ONAQ', refGas = 'low')))
#   expect_true(is.list(calibrate_standards_carbon(cal_lr, low, correct_bad_refvals = TRUE, site = 'WOOD', refGas = 'low')))
#   expect_true(is.list(calibrate_standards_carbon(cal_lr, low, correct_bad_refvals = TRUE, site = 'BLAN', refGas = 'low')))
#   expect_true(is.list(calibrate_standards_carbon(cal_lr, low, correct_bad_refvals = TRUE, site = 'STER', refGas = 'low')))
#   expect_true(is.list(calibrate_standards_carbon(cal_lr, low, correct_bad_refvals = TRUE, site = 'ORNL', refGas = 'low')))
#   expect_true(is.list(calibrate_standards_carbon(cal_lr, low, correct_bad_refvals = TRUE, site = 'TREE', refGas = 'low')))
#   expect_true(is.list(calibrate_standards_carbon(cal_lr, low, correct_bad_refvals = TRUE, site = 'BARR', refGas = 'low')))
#   expect_true(is.list(calibrate_standards_carbon(cal_lr, low, correct_bad_refvals = TRUE, site = 'SRER', refGas = 'low')))
#   expect_true(is.list(calibrate_standards_carbon(cal_lr, low, correct_bad_refvals = TRUE, site = 'HARV', refGas = 'low')))
# })