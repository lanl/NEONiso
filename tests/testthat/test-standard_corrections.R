
ref_data <- data.frame(d2H_ref_mean  = c(-80,-100,-120),
                       d18O_ref_mean = c(-10,-11,-12))

test_that("reference water with reasonable d-excess is unchanged", {
  expect_equal(swap_standard_isotoperatios(ref_data),ref_data)
})

ref_data2 <- data.frame(d2H_ref_mean = c(-10,-11,-12),
                        d18O_ref_mean = c(-80,-100,-120))

test_that("unreasonable d-excess values get swapped", {
  expect_equal(swap_standard_isotoperatios(ref_data2),ref_data)
})

test_vals <- c(0,-5,-10,-20,-30)

test_that("R_to_delta and delta_to_R are invertible", {
  expect_equal(R_to_delta(delta_to_R(test_vals,"carbon"),"carbon"),test_vals)
  expect_equal(R_to_delta(delta_to_R(test_vals,"oxygen"),"oxygen"),test_vals)
  expect_equal(R_to_delta(delta_to_R(test_vals,"hydrogen"),"hydrogen"),test_vals)
})

