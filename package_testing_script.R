# package_testing_script.R
# rich fiorella
# 210310

# thanks for helping out to test the NEONiso package.
# the following script should help test all of the functions
# on different systems, as well as the vignette that comes 
# with the package.

# please let me know if each function here runs successfully
# it would also be helpful to know: a) what OS you're running 
# and b) what version of R you're using

# install development branch of the package
devtools::install_github('SPATIAL-Lab/NEONiso', 
                         ref = 'simplify-carbon-routines',
                         build_vignettes = TRUE, force = TRUE)
library(NEONiso)

#-------------------------------------------------------------------
# OPTIONAL: there's a chance R may complain about not being 
# able to find rhdf5 (it's not on CRAN). if this is the case, try this:
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("rhdf5")
#------------------------------------------------------------------

# check to make sure the vignette is available
vignette('example_workflow', package = 'NEONiso')

# test a simple workflow (this is also in the vignette)

your_path <- '~/Desktop/' # set a path where data will be saved on your computer

manage_local_EC_archive(file_dir = your_path, get = TRUE, unzip_files = TRUE, sites = "ONAQ") # this will take several minutes, 
                                                                                              # download a few GB of data

#check your path after this has finished running - are there a series of (monthly) hdf5 files for ONAQ?
# if so, try calibrating data:
fnames <- list.files(path = paste0(your_path,'/ONAQ'),
                     pattern = '.h5',
                     recursive = TRUE,
                     full.names = TRUE)
# unselect gz files.
fnames <- fnames[!grepl('.gz',fnames)]

fname.byfolder <- strsplit(fnames, split=".", fixed = TRUE)
site.code  <- sapply(fname.byfolder,'[[',3)

# inspect site.code in the environment: is it a vector that with repeated "ONAQ"?

fnames.tmp <- gsub(".h5",".calibrated.h5",fnames)
fnames.spt <- strsplit(fnames.tmp, split = "/")
fnames.out <- sapply(fnames.spt, '[[', 7)

# create new output directory
outpaths   <- paste0(your_path,'/ONAQ/output/')
sapply(unique(outpaths),dir.create,showWarnings=FALSE) # apply function used here to generalize in case you wanted to run all sites

# update fnames.out to include desired output paths.
fnames.out <- paste0(outpaths,"/",fnames.out)

# try calibrating data:
for (i in 1:length(fnames.out)) {
  calibrate_carbon_bymonth(fnames[i], fnames.out[i], site=site.code[i], method = "Bowling_2003")
}
