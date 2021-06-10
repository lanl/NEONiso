# test-data_ingestion

fin <- system.file('extdata',
                   'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5',
                   package = 'NEONiso', mustWork = TRUE)

#------------------------------------------
# various tests about output structure of ingest_data.

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


