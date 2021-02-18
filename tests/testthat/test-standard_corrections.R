
ref_data <- data.frame(d2H_ref_mean  = c(-80,-100,-120),
                       d18O_ref_mean = c(-10,-11,-12))

test_that("reference water with reasonable d-excess is unchanged", {
  expect_equal(swap_standard_isotoperatios(ref_data),ref_data)
})

ref_data2 <- data.frame(d18O_ref_mean  = c(-80,-100,-120),
                       d2H_ref_mean = c(-10,-11,-12))

test_that("unreasonable d-excess values get swapped", {
  expect_equal(swap_standard_isotoperatios(ref_data2),ref_data)
})
