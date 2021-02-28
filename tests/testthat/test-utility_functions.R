# test time conversion function
test_that("convert_POSIXct_to_NEONhdf5_time returns a string 24 characters long", {
  expect_type(convert_POSIXct_to_NEONhdf5_time(Sys.time()), "character")
  expect_equal(nchar(convert_POSIXct_to_NEONhdf5_time(Sys.time())), 24)
})


test_that("site retrieval functions return vector of strings", {
  expect_type(terrestrial_core_sites(), "character")
  expect_type(terrestrial_relocatable_sites(), "character")
  expect_type(water_isotope_sites(), "character")
})

test_that("correct number of sites in each site retrieval function vector",{
  expect_equal(length(terrestrial_core_sites()), 20)
  expect_equal(length(terrestrial_relocatable_sites()), 27)
  expect_equal(length(water_isotope_sites()), 21)
})