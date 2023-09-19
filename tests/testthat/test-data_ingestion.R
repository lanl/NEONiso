# test-data_ingestion

fin <- system.file("extdata",
                   "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
                   package = "NEONiso", mustWork = TRUE)

#------------------------------------------
# various tests about output structure of ingest_data.

# TEMPORARY TESTS:
# expect failure if analyte = "H2o"
test_that("ingest_data errors for H2o (TEMPORARY)", {
  expect_error(ingest_data(fin, analyte = "H2o"))
})

# 1) for CO2:
co2test <- ingest_data(fin, analyte = "Co2", avg = 9)

# a) that a list is returned
test_that("ingest_data returns a list for analyte='Co2'", {
  expect_type(co2test, "list")
})

# b) that list contains appropriate list of lists
test_that("ingest_data returns list with correct sublist names", {
  expect_equal(names(co2test), c("ambient", "reference", "refe_stacked"))
})

# test restructure_carbon_variables function
rest_raw <- neonUtilities::stackEddy(fin, avg = 9, level = "dp01")[[1]] %>%
  dplyr::select("verticalPosition", "timeBgn",
                "timeEnd", tidyselect::contains("isoCo2"))

rest_raw <- rest_raw[rowSums(is.na(rest_raw)) < 145, ] # Needed to remove merge error where times are slightly different between h2o and co2.

test_that("restructure_carbon_variables errors when invalid mode provided", {
  expect_error(restructure_carbon_variables(rest_raw,
                                            "dlta13CCo2",
                                            mode = "cheese",
                                            group = "data"))
  expect_silent(restructure_carbon_variables(rest_raw,
                                             "dlta13CCo2",
                                             mode = "reference",
                                             group = "data"))
  expect_silent(restructure_carbon_variables(rest_raw,
                                             "dlta13CCo2",
                                             mode = "ambient",
                                             group = "data"))
})

test_that("output structure from restructure_carbon_variables is correct", {
  expect_true(is.data.frame(restructure_carbon_variables(rest_raw,
                                                         "dlta13CCo2",
                                                         mode = "reference",
                                                         group = "data")))
  expect_equal(ncol(restructure_carbon_variables(rest_raw,
                                                 "dlta13CCo2",
                                                 mode = "reference",
                                                 group = "data")), 7)
  expect_equal(ncol(restructure_carbon_variables(rest_raw,
                                                 "dlta13CCo2",
                                                 mode = "ambient",
                                                 group = "data")), 7)
})

test_that("output datetimes from restructure_carbon_variables are characters, not POSIX", {
  tmp <- restructure_carbon_variables(rest_raw,
                                      "dlta13CCo2",
                                      mode = "ambient",
                                      group = "data")
  expect_type(tmp$timeBgn, "character")
  expect_type(tmp$timeEnd, "character")
  tmp <- restructure_carbon_variables(rest_raw,
                                      "dlta13CCo2",
                                      mode = "reference",
                                      group = "data")
  expect_type(tmp$timeBgn, "character")
  expect_type(tmp$timeEnd, "character")
})

#--------------------------------------------------
fin1 <- system.file("extdata",
                    "NEON.D15.ONAQ.DP4.00200.001.nsae.2020-06-01.basic.packed.h5",
                    package = "NEONiso", mustWork = TRUE)
fin2 <- system.file("extdata",
                    "NEON.D15.ONAQ.DP4.00200.001.nsae.2020-06-02.basic.packed.h5",
                    package = "NEONiso", mustWork = TRUE)
fin3 <- system.file("extdata",
                    "NEON.D15.ONAQ.DP4.00200.001.nsae.2020-06-03.basic.packed.h5",
                    package = "NEONiso", mustWork = TRUE)

# create list of files
all_files <- c(fin1, fin2, fin3)

test1 <- ingest_data(fin1, analyte = "Co2", avg = 9)
test2 <- ingest_data(all_files, analyte = "Co2", avg = 9)

test_that("stackEddy/ingest data works on multiple daily files", {
  # would expect number of rows to be larger in test2 than test 1:
  expect_gt(nrow(test2$ambient$`000_010_09m`$dlta13CCo2), nrow(test1$ambient$`000_010_09m`$dlta13CCo2))
})

