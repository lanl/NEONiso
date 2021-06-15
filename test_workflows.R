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
data.dir <- '~/Desktop/DP4_00200_001/'

# make output directory structure:
dir.create(paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests"))

# which tests to run?
run_test1 <- TRUE
run_test2 <- TRUE
run_test3 <- FALSE
run_test4 <- FALSE
run_test5 <- FALSE
run_test6 <- FALSE
run_test7 <- FALSE
run_test8 <- TRUE

# load required packages:
library(rhdf5)
library(dplyr)
library(lubridate)

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
  fnames.tmp <- sapply(fnames.lst,'[[',8)

  fnames.out <- gsub(".h5",".calibrated.h5",fnames.tmp)
}

if (run_test4 | run_test6) {
  csites <- c(NEONiso:::terrestrial_core_sites(), NEONiso:::terrestrial_relocatable_sites())
  fin <- list.files(paste0(data.dir,csites[i],'/'),full.names=TRUE)
  fout <- list.files(paste0(data.dir,csites[i],'/'),full.names=FALSE)
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
  
  limitSite <- "BARR"
  wnames <- wnames[wsite.code == limitSite]
  wsite.yearmonth <- wsite.yearmonth[wsite.code == limitSite]
  wsite.domain <- wsite.domain[wsite.code == limitSite]
  wsite.code <- wsite.code[wsite.code == limitSite]
  
  # get names only.
  wnames.lst <- strsplit(wnames,split="/")
  wnames.tmp <- sapply(wnames.lst,'[[',8)
  
  wnames.out <- gsub(".h5",".calibrated.h5",wnames.tmp)
}

if (run_test8) {
  wsites <- NEONiso:::water_isotope_sites()
  fwin <- list.files(paste0(data.dir,wsites[i],'/'),full.names=TRUE)
  fwout <- list.files(paste0(data.dir,wsites[i],'/'),full.names=FALSE)
}

#==========================
# Run carbon tests
#==========================

if (run_test1) {
  dir.create(paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/01"))

  outpaths <- paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/01/", site.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  fnames.out2 <- paste0(outpaths,"/",fnames.out)
  
  for (i in 1:length(fnames.out)) {
     print(paste0("Calibration test set 1: ", round(100*i/length(fnames.out),3),"% complete"))
     calibrate_carbon_bymonth(fnames[i],fnames.out2[i],site=site.code[i], method = "Bowling_2003")
  }
  
  # cleanup
  rm(outpaths, fnames.out2)
  
}

if (run_test2) {
  dir.create(paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/02"))
  
  outpaths <- paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/02/", site.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  fnames.out2 <- paste0(outpaths,"/",fnames.out)
  
  for (i in 1:length(fnames.out)) {
    print(paste0("Calibration test set 2: ", round(100*i/length(fnames.out),3),"% complete"))
    calibrate_carbon_bymonth(fnames[i],fnames.out2[i],
                             site=site.code[i], method = "linreg")
  }

  # cleanup
  rm(outpaths, fnames.out2)
}

#----------------------------------------------------
# calibrate_carbon tests:
#----------------------------------------------------

if (run_test3) {
  dir.create(paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/03"))
  
  outpaths <- paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/03/", site.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  fnames.out2 <- paste0(outpaths,"/",fnames.out)
  
  for (i in 1:length(fnames.out)) {
    print(paste0("Calibration test set 3: ", round(100*i/length(fnames.out),3),"% complete"))
    calibrate_carbon(fnames[i],fnames.out2[i],site=site.code[i], method = "Bowling_2003")
  }
}

if (run_test4) {
  dir.create(paste0('~/Desktop/NEONcal/',Sys.Date(),'_tests/04'))
  
  for (i in 1:length(csites)) {
    calibrate_carbon(fin,
                     paste0('~/Desktop/NEONcal/',Sys.Date(),'_tests/04/',fout[1]),
                     site=csites[i], r2_thres = 0.95,
                     calibration_half_width = 100000)
  }

}

if (run_test5) {
  dir.create(paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/05"))
  
  outpaths <- paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/05/", site.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  fnames.out2 <- paste0(outpaths,"/",fnames.out)
  
  for (i in 1:length(fnames.out)) {
    print(paste0("Calibration test set 5: ", round(100*i/length(fnames.out),3),"% complete"))
    calibrate_carbon(fnames[i],fnames.out2[i],site=site.code[i], method = "linreg")
  }
}

if (run_test6) {
  dir.create(paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/06"))
  
  
  for (i in 1:length(csites)) {
    calibrate_carbon(fin,
                     paste0('~/Desktop/NEONcal/',Sys.Date(),'_tests/06/',fout[1]),
                     site=csites[i], r2_thres = 0.95, method = 'linreg',
                     calibration_half_width = 100000)
  }
}


#----------------------------------------------------
# Water tests:
#----------------------------------------------------
# calibrate_water_bymonth test:
#----------------------------------------------------

if (run_test7) {
  dir.create(paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/07"))
  
  outpaths <- paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/07/", wsite.code)
  
  sapply(unique(outpaths),dir.create,showWarnings=FALSE)
  wnames.out2 <- paste0(outpaths,"/",wnames.out)
  
  for (i in 1:length(wnames)) {
    calibrate_water_linreg(wnames[i],
                           wnames.out2[i],
                           site=wsite.code[i])
  }  
  
}

#----------------------------------------------------
# calibrate_water_bysite test:
#----------------------------------------------------

if (run_test8) {
  dir.create(paste0('~/Desktop/NEONcal/',Sys.Date(),"_tests/08"))
  
  for (i in 1:length(wsites)) {
    calibrate_water_linreg_bysite(paste0(data.dir,wsites[i],'/'),
                                  paste0('~/Desktop/NEONcal/',Sys.Date(),'_tests/08/'),
                                  site=wsites[i], r2_thres = 0.95,
                                 calibration_half_width = 100000)
  }  
}
