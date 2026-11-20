zoo_reference <- function(data, width = 7, threshold = 5) {
  filt <- zoo::rollapply(data, width, median, na.rm = TRUE, fill = NA)
  spikes <- abs(data - filt) > threshold
  data[spikes == TRUE] <- NA
  data
}

test_that("filter_median_brock86 removes isolated spikes", {
  x <- c(rep(-8, 10), 30, rep(-8, 10))
  out <- filter_median_brock86(x)
  expect_true(is.na(out[11]))
  expect_identical(out[-11], x[-11])
})

test_that("filter_median_brock86 matches zoo reference on NA-heavy data", {
  set.seed(1)
  for (p_na in c(0, 0.1, 0.5, 0.9, 1)) {
    x <- rnorm(500, -8, 3)
    x[runif(500) < p_na] <- NA
    x[sample(500, 5)] <- 40
    expect_identical(filter_median_brock86(x), zoo_reference(x))
  }
})

test_that("filter_median_brock86 handles short and special inputs", {
  expect_identical(filter_median_brock86(c(1, 2, 3)), c(1, 2, 3))
  expect_identical(filter_median_brock86(numeric(0)), numeric(0))
  x <- c(NaN, Inf, -Inf, rnorm(20), NA)
  expect_identical(filter_median_brock86(x), zoo_reference(x))
})
