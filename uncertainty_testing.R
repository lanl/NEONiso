# uncertainty_testing.R
# rich fiorella
# 9 december 2021
# load required packages:
library(rhdf5)
library(dplyr)
library(lubridate)
library(doParallel)
library(foreach)
# script to drive multiple tests of half_width size.

# where does uncalibrated data live?
data.dir <- '~/NEON/DP4_00200_001/'

which_test <- 'medlow'

# set test_date
# make output directory structure:
dir.create(paste0('~/NEONcal/carbon_halfwidth_tests/',which_test), recursive = TRUE)

csites <- c(NEONiso:::terrestrial_core_sites(), NEONiso:::terrestrial_relocatable_sites())

# set up a function to drive calibration across sites:

test_half_width <- function(ndays) {
  dir.create(paste0('~/NEONcal/carbon_halfwidth_tests/',which_test,'/',ndays))
  
  for (i in 1:length(csites)) {
    
    fin <- list.files(paste0(data.dir,csites[i],'/') , pattern = '.h5', full.names=TRUE)
    fout <- list.files(paste0(data.dir,csites[i],'/'), pattern = '.h5', full.names=FALSE)
    
    calibrate_carbon(fin,
                     paste0(paste0('~/NEONcal/carbon_halfwidth_tests/',which_test,'/',ndays,'/'),fout[1]),
                     site=csites[i], r2_thres = 0.95,
                     calibration_half_width = ndays)
  }
}

# set up a cluster to run across possible nday options
nday_list <- c(1, 2, 3, 4, 7, 14, 21, 28, 90, 180, 365, 100000)

local.cluster <- parallel::makeCluster(12, type = "FORK")
doParallel::registerDoParallel(cl = local.cluster)
foreach::getDoParRegistered()

foreach (i = 1:12) %dopar% {test_half_width(nday_list[i])}

stopCluster(local.cluster)

