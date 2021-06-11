# test-data_ingestion

fin <- system.file('extdata',
                   'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5',
                   package = 'NEONiso', mustWork = TRUE)

#------------------------------------------
# various tests about output structure of ingest_data.

# TEMPORARY TESTS:
# expect failure if analyte = 'H2o'
test_that("ingest_data errors for H2o (TEMPORARY)", {
  expect_error(ingest_data(fin, analyte = 'H2o'))
})

# 1) for CO2:
co2test <- ingest_data(fin, analyte = 'Co2')

# a) that a list is returned
test_that("ingest_data returns a list for analyte='Co2'", {
  expect_type(co2test, "list")
})

# b) that list contains appropriate list of lists
test_that("ingest_data returns list with correct sublist names", {
  expect_equal(names(co2test), c("ambient", "reference", "refe_stacked"))
})

# test restructure_carbon_variables function
rest_raw <- neonUtilities::stackEddy(fin, avg = 9, level = 'dp01')[[1]] %>%
  dplyr::select(.data$verticalPosition, .data$timeBgn, 
                .data$timeEnd, tidyselect::contains('isoCo2'))

rest_raw <- rest_raw[rowSums(is.na(rest_raw)) < 145, ] # Needed to remove merge error where times are slightly different between h2o and co2.

test_that("restructure_carbon_variables errors when invalid mode provided", {
  expect_error(restructure_carbon_variables(rest_raw, 'dlta13CCo2', mode = 'cheese', group = 'data'))
  expect_silent(restructure_carbon_variables(rest_raw, 'dlta13CCo2', mode = 'reference', group = 'data'))
  expect_silent(restructure_carbon_variables(rest_raw, 'dlta13CCo2', mode = 'ambient', group = 'data'))
})

test_that("output structure from restructure_carbon_variables is correct", {
  expect_true(is.data.frame(restructure_carbon_variables(rest_raw, 'dlta13CCo2', mode = 'reference', group = 'data')))
  expect_equal(ncol(restructure_carbon_variables(rest_raw, 'dlta13CCo2', mode = 'reference', group = 'data')), 7)
  expect_equal(ncol(restructure_carbon_variables(rest_raw, 'dlta13CCo2', mode = 'ambient', group = 'data')), 7)
})

test_that("output datetimes from restructure_carbon_variables are characters, not POSIX", {
  tmp <- restructure_carbon_variables(rest_raw, 'dlta13CCo2', mode = 'ambient', group = 'data')
  expect_type(tmp$timeBgn, 'character')
  expect_type(tmp$timeEnd, 'character')
  tmp <- restructure_carbon_variables(rest_raw, 'dlta13CCo2', mode = 'reference', group = 'data')
  expect_type(tmp$timeBgn, 'character')
  expect_type(tmp$timeEnd, 'character')
})

