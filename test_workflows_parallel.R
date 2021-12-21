# test_workflows.R
# rich fiorella 210616

# run suite of tests for each version to make sure
# that calibration scripts work, and id any problems.
# tests to run:
# 1) calibrate_carbon_bymonth works B03 method (soon to be deprecated)
# 2) calibrate_carbon_bymonth works linreg method (soon to be deprecated)
# 3) calibrate_carbon works for monthly files, B03 method
# 4) calibrate_carbon works at a site level, B03 method
# 5) calibrate_carbon works for monthly files, linreg method
# 6) calibrate_carbon works at a site level, linreg method
# 7) calibrate_water_linreg_bymonth works
# 8) calibrate_water_linreg_bysite works

# where does uncalibrated data live?
data.dir <- '~/DP4_00200_001/'

# set test_date
test_date <- "2021-12-20"

# make output directory structure:
dir.create(paste0('~/NEONcal/',test_date,"_tests"))

# which tests to run?
run_test1 <- TRUE
run_test2 <- TRUE
run_test3 <- TRUE
run_test4 <- TRUE
run_test5 <- TRUE
run_test6 <- TRUE
run_test7 <- FALSE
run_test8 <- FALSE
rapid_test <- FALSE # if rapid, only run ~5% of possible site months.

# load required packages:
library(rhdf5)
library(dplyr)
library(lubridate)
#library(doParallel)
#library(foreach)
library(parallel)

devtools::load_all()

#----------------------------------------------------
# Carbon tests:
#----------------------------------------------------
# calibrate_carbon_bymonth tests:
#----------------------------------------------------

#========================================
# Determine filenames, function arguments, etc.
#========================================
# by month

if (run_test1 | run_test2 | run_test3 | run_test5) {
  
  fnames <- list.files(path=data.dir,
                       pattern='.h5',
                       recursive=TRUE,
                       full.names=TRUE)
  
  fnames <- fnames[!grepl('.gz',fnames)]
  
  fname.byfolder <- strsplit(fnames,split=".",fixed=T)
  
  site.code <- sapply(fname.byfolder,'[[',3)
  site.yearmonth <- sapply(fname.byfolder,'[[',8)
  site.domain <- sapply(fname.byfolder,'[[',2)
  
  #limitSite <- "BARR"
  #fnames <- fnames[site.code == limitSite]
  #site.yearmonth <- site.yearmonth[site.code == limitSite]
  #site.domain <- site.domain[site.code == limitSite]
  #site.code <- site.code[site.code == limitSite]
  
  # get names only.
  fnames.lst <- strsplit(fnames,split="/")
  fnames.tmp <- sapply(fnames.lst,'[[',7)

  fnames.out <- gsub(".h5",".calibrated.h5",fnames.tmp)
}

if (run_test4 | run_test6) {
  csites <- c(NEONiso:::terrestrial_core_sites(), NEONiso:::terrestrial_relocatable_sites())
}

# water files
if (run_test7) {
  # need to filter based on whether it's a water isotope site or not.
  
  wnames <- list.files(path=data.dir,
                       pattern='.h5',
                       recursive=TRUE,
                       full.names=TRUE)
  
  wnames <- wnames[!grepl('.gz',wnames)]
  
  wname.byfolder <- strsplit(wnames,split=".",fixed=T)
  
  wsite.code <- sapply(wname.byfolder,'[[',3)
  wsite.yearmonth <- sapply(wname.byfolder,'[[',8)
  wsite.domain <- sapply(wname.byfolder,'[[',2)
  
  # limitSite <- "BARR"
  # wnames <- wnames[wsite.code == limitSite]
  # wsite.yearmonth <- wsite.yearmonth[wsite.code == limitSite]
  # wsite.domain <- wsite.domain[wsite.code == limitSite]
  # wsite.code <- wsite.code[wsite.code == limitSite]
  
  # get names only.
  wnames.lst <- strsplit(wnames,split="/")
  wnames.tmp <- sapply(wnames.lst,'[[',7)
  
  wnames.out <- gsub(".h5",".calibrated.h5",wnames.tmp)
}

if (run_test8) {
  wsites <- NEONiso:::water_isotope_sites()
}

# if we're doing rapid testing!
if (rapid_test) {
  fnames.out <- sample(fnames.out, floor(length(fnames.out)/20))
  csites <- sample(csites, 4)
}

# set up parallel cluster
local.cluster <- parallel::makeCluster(20, type = "FORK")
doParallel::registerDoParallel(cl = local.cluster)
foreach::getDoParRegistered()

#==========================
# Run carbon tests
#==========================


if (run_test1) {
  print(paste(Sys.time(), "starting test 1"))
  dir.create(paste0('~/NEONcal/',test_date,"_tests/01"))

  outpaths <- paste0('~/NEONcal/',test_date,"_tests/01/", site.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  fnames.out2 <- paste0(outpaths,"/",fnames.out)
  
  mclapply(seq_along(fnames), function(i){
    print(paste0("Calibration test set 1: ", round(100*i/length(fnames.out),3),"% complete"))
    calibrate_carbon_bymonth(fnames[i],fnames.out2[i],site=site.code[i], method = "Bowling_2003")
  },
  mc.cores = 20)
  
  # cleanup
  rm(outpaths, fnames.out2)
  print(paste(Sys.time(), "ending test 1"))
  
}

if (run_test2) {
  print(paste(Sys.time(), "starting test 2"))
  dir.create(paste0('~/NEONcal/',test_date,"_tests/02"))
  
  outpaths <- paste0('~/NEONcal/',test_date,"_tests/02/", site.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  fnames.out2 <- paste0(outpaths,"/",fnames.out)
  
  mclapply(seq_along(fnames), function(i){
    print(paste0("Calibration test set 2: ", round(100*i/length(fnames.out),3),"% complete"))
    calibrate_carbon_bymonth(fnames[i],fnames.out2[i],
                             site=site.code[i], method = "linreg")
  },
  mc.cores = 20)

  # cleanup
  rm(outpaths, fnames.out2)
  print(paste(Sys.time(), "ending test 2"))
}

#----------------------------------------------------
# calibrate_carbon tests:
#----------------------------------------------------

if (run_test3) {
  print(paste(Sys.time(), "starting test 3"))
  dir.create(paste0('~/NEONcal/',test_date,"_tests/03"))
  
  outpaths <- paste0('~/NEONcal/',test_date,"_tests/03/", site.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  fnames.out2 <- paste0(outpaths,"/",fnames.out)
  
  mclapply(seq_along(fnames), function(i){
    print(paste0("Calibration test set 3: ", round(100*i/length(fnames.out),3),"% complete...", fnames.out[i]))
    calibrate_carbon(fnames[i],fnames.out2[i],site=site.code[i], method = "Bowling_2003")
  }, mc.cores = 20)
  
  print(paste(Sys.time(), "ending test 3"))
}

if (run_test4) {
  dir.create(paste0('~/NEONcal/',test_date,'_tests/04'))
  
  
  print(paste(Sys.time(), "starting test 4"))
  mclapply(seq_along(csites), function(i) {
    
    fin <- list.files(paste0(data.dir,csites[i],'/') , pattern = '.h5', full.names=TRUE)
    fout <- list.files(paste0(data.dir,csites[i],'/'), pattern = '.h5', full.names=FALSE)
    
    calibrate_carbon(fin,
                     paste0('~/NEONcal/',test_date,'_tests/04/',fout[1]),
                     site=csites[i], r2_thres = 0.95,
                     calibration_half_width = 100000)
  },
  mc.cores = 20)
  
  print(paste(Sys.time(), "ending test 4"))
}

if (run_test5) {
  
  print(paste(Sys.time(), "starting test 5"))
  
  dir.create(paste0('~/NEONcal/',test_date,"_tests/05"))
  
  outpaths <- paste0('~/NEONcal/',test_date,"_tests/05/", site.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  fnames.out2 <- paste0(outpaths,"/",fnames.out)
  
  mclapply(seq_along(fnames), function(i) {
    print(paste0("Calibration test set 5: ", round(100*i/length(fnames.out),3),"% complete"))
    calibrate_carbon(fnames[i],fnames.out2[i],site=site.code[i], method = "linreg")
  },
  mc.cores = 20)

  print(paste(Sys.time(), "ending test 5"))
}

if (run_test6) {
  
  print(paste(Sys.time(), "starting test 6"))
  
  dir.create(paste0('~/NEONcal/',test_date,"_tests/06"))
  
  mclapply(seq_along(csites), function(i) {
    fin <- list.files(paste0(data.dir,csites[i],'/'), pattern = '.h5', full.names=TRUE)
    fout <- list.files(paste0(data.dir,csites[i],'/'), pattern = '.h5', full.names=FALSE)
    
    calibrate_carbon(fin,
                     paste0('~/NEONcal/',test_date,'_tests/06/',fout[1]),
                     site=csites[i], r2_thres = 0.95, method = 'linreg',
                     calibration_half_width = 100000)
  },
  mc.cores = 20)
  
  print(paste(Sys.time(), "ending test 6"))
  
}


#----------------------------------------------------
# Water tests:
#----------------------------------------------------
# calibrate_water_bymonth test:
#----------------------------------------------------

if (run_test7) {
  dir.create(paste0('~/NEONcal/',test_date,"_tests/07"))
  
  outpaths <- paste0('~/NEONcal/',test_date,"_tests/07/", wsite.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  wnames.out2 <- paste0(outpaths,"/",wnames.out)
  
  foreach (i = 1:length(wnames), .packages = "NEONiso") %dopar% {
    NEONiso:::calibrate_water_linreg_bymonth(wnames[i],
                           wnames.out2[i],
                           site=wsite.code[i])
  }  
  
}

#----------------------------------------------------
# calibrate_water_bysite test:
#----------------------------------------------------

if (run_test8) {
  dir.create(paste0('~/NEONcal/',test_date,"_tests/08"))
  
  foreach (i = 1:length(wsites)) %dopar% {
    calibrate_water_linreg_bysite(paste0(data.dir,wsites[i],'/'),
                                  paste0('~/NEONcal/',test_date,'_tests/08/'),
                                  site=wsites[i], r2_thres = 0.95,
                                 calibration_half_width = 100000)
  }  
}

stopCluster(local.cluster)
