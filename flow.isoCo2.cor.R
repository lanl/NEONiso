install.packages("NEONiso")
library(NEONiso)
library(rhdf5)
#workflow to correct G2131-i data

#NEON site
site <- "ONAQ"

#working directory path
dir <- paste0("~/eddy/data/tmp/",site)
#input data directory path
inpDir <- paste0(dir,"/inp")
  
#list all file names in inpDir
nameFile <- list.files(path = inpDir,
                       pattern = '.h5',
                       recursive = TRUE,
                       full.names = TRUE)
#Output file names
nameOutFileTmp <- gsub(".h5",".calibrated.h5",nameFile)
nameOutFileSplt <- strsplit(nameOutFileTmp, split = "/")
#get output file names
nameOutFileOut <- sapply(nameOutFileSplt, '[[', length(nameOutFileSplt[[1]]))

#output data directory path
outDir <- paste0(dir,"/out")
#output file names
nameOutFileOut <- paste0(outDir,"/",nameOutFileOut)

#correction processing
for (i in 1:length(nameOutFileOut)) {
  calibrate_carbon_bymonth(nameFile[i],nameOutFileOut[i],site=site, method = "Bowling_2003")
}

