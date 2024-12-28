# test-high_level_functions

# generally test the calibration functions, with write_to_file = FALSE
# in general, it is hard to see how these should return an error if
# all other tests pass, but good practice to include these in case a
# stray character enters these functions somehow.

# 210630: these tests actually don't do much right now, other than
# increase coverage!
# could spend some time actually having these test important functionality.

fin <- system.file("extdata",
                   "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
                   package = "NEONiso", mustWork = TRUE)

fout1 <- tempfile()
fout2 <- tempfile()
fout3 <- tempfile()

test_that("calibrate_carbon returns no error", {

  skip_on_cran()
  # these tests could probably be made more useful!!
  
  expect_no_error(calibrate_carbon(fin, fout1, "ONAQ",
                                  method = "gainoffset",
                                  calibration_half_width = 0.5,
                                  force_cal_to_beginning = TRUE,
                                  force_cal_to_end = TRUE,
                                  gap_fill_parameters = FALSE,
                                  filter_ambient = TRUE,
                                r2_thres = 0.95,
                                  correct_ref_data = TRUE,
                                  write_to_file = TRUE))

  expect_no_error(calibrate_carbon(fin, fout2, "ONAQ",
                                   method = "linreg",
                                   calibration_half_width = 0.5,
                                   force_cal_to_beginning = TRUE,
                                   force_cal_to_end = TRUE,
                                   gap_fill_parameters = FALSE,
                                   filter_ambient = TRUE,
                                   r2_thres = 0.95,
                                   correct_ref_data = TRUE,
                                   write_to_file = TRUE))

  # these tests could probably be made more useful!!
  expect_no_error(calibrate_carbon(fin, "/dev/null", "ONAQ",
                                   method = "gainoffset",
                                   calibration_half_width = 0.5,
                                   force_cal_to_beginning = TRUE,
                                   force_cal_to_end = TRUE,
                                   gap_fill_parameters = FALSE,
                                   filter_ambient = TRUE,
                                   r2_thres = 0.95,
                                   correct_ref_data = TRUE,
                                   write_to_file = FALSE))
  
  expect_warning(calibrate_carbon(fin, "/dev/null", "ONAQ",
                                  method = "Bowling_2003",
                                  calibration_half_width = 0.5,
                                  force_cal_to_beginning = TRUE,
                                  force_cal_to_end = TRUE,
                                  gap_fill_parameters = FALSE,
                                  filter_ambient = TRUE,
                                  r2_thres = 0.95,
                                  correct_ref_data = TRUE,
                                  write_to_file = FALSE))
})


fout3 <- tempfile()

test_that("calibrate_water returns no error", {

  skip_on_cran()
  # these tests could probably be made more useful!!
  expect_no_error(calibrate_water(fin, "/dev/null", "ONAQ",
                                  correct_ref_data = TRUE,
                                  write_to_file = FALSE))

  expect_no_error(calibrate_water(fin, fout3, "ONAQ",
                                  correct_ref_data = TRUE,
                                  write_to_file = TRUE))

})
