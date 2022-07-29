# make ref corrections data - 
# converts xlsx to data file.

library(readxl)

# carbon: open xlsx file, and any formatting needed?
carb <- read_xlsx("refCorrections.xlsx")

# save as data file to R/
usethis::use_data(carb, internal = TRUE)
