# extract_data_for_Gabe.R
# ONAQ data for Gabe and Paige.

library(neonUtilities)
library(dplyr)
library(magrittr)

data <- neonUtilities::stackEddy("~/Downloads/NEON.D15.ONAQ.DP4.00200.001.nsae.all.basic.wiso.calibrated.2e+05dayWindow.h5",
                                 level = 'dp01',
                                 avg = 9)

data_red <- data[["ONAQ"]] %>%
  dplyr::select(verticalPosition,timeBgn,timeEnd,data.isoH2o.dlta18OH2o.mean_cal,
                data.isoH2o.dlta2HH2o.mean_cal, data.isoH2o.rtioMoleDryH2o.mean,
                data.isoH2o.rtioMoleWetH2o.mean) %>%
  dplyr::rename(d18O=data.isoH2o.dlta18OH2o.mean_cal,
                d2H =data.isoH2o.dlta2HH2o.mean_cal, 
                spec_hum=data.isoH2o.rtioMoleWetH2o.mean,
                mixing_ratio=data.isoH2o.rtioMoleDryH2o.mean)

# get heights
attrs <- rhdf5::h5readAttributes("~/Downloads/NEON.D15.ONAQ.DP4.00200.001.nsae.all.basic.wiso.calibrated.2e+05dayWindow.h5", "ONAQ")

heights <- as.numeric(attrs$DistZaxsLvlMeasTow)

data_red <- data_red %>%
  dplyr::mutate(level = as.numeric(verticalPosition)/10,
                height = heights[level]) %>%
  dplyr::select(-verticalPosition) %>%
  dplyr::filter(lubridate::year(timeBgn) >= 2020 & lubridate::year(timeEnd) < 2021)

# round delta, q, and w:
data_red$d18O <- round(data_red$d18O, digits = 2)
data_red$d2H <- round(data_red$d2H, digits = 2)
data_red$mixing_ratio <- round(data_red$mixing_ratio, digits = 3)
data_red$spec_hum <- round(data_red$spec_hum, digits = 3)

write.table(data_red, file = "~/Desktop/ONAQ_wiso_2020.csv", sep = ',', row.names = FALSE)
