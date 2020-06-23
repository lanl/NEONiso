# general utility functions in this file.
# this file is intended to keep short utilitie functions
# that do *not* need to be exported,  to help keep down on
# number of files present in the repo/package. -rpf 200214.
# 200219 - added a water isotope function.
#--------------------------------------------------------------
#' convert_POSIXct_to_NEONhdf5_time
#'
#' Converts a POSIXct object back to the character format used by NEON in their
#' HDF eddy covariance files. Output format,  using strptime syntax,  is
#' %Y-%m-%dT%H:%M:%OSZ.
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param intime POSIXct vector to convert to NEON time format.
#'
#' @return Returns character version of POSIXct object
#'         matching NEON time variable format.
#'
#'
convert_POSIXct_to_NEONhdf5_time <- function(intime) {

  # convert from POSIXct to
  # a time in YYmmddTHH:MM:SSSZ format used by NEON hdf5 files.
  outtime <- as.character(paste0(lubridate::year(intime), "-",
                ifelse(lubridate::month(intime) < 10,
                       paste0("0", lubridate::month(intime)),
                       lubridate::month(intime)), "-",
                ifelse(lubridate::day(intime) < 10,
                       paste0("0", lubridate::day(intime)),
                       lubridate::day(intime)), "T",
                ifelse(lubridate::hour(intime) < 10,
                       paste0("0", lubridate::hour(intime)),
                       lubridate::hour(intime)), ":",
                ifelse(lubridate::minute(intime) < 10,
                       paste0("0", lubridate::minute(intime)),
                       lubridate::minute(intime)), ":",
                ifelse(lubridate::second(intime) < 10,
                       paste0("0", lubridate::second(intime), ".000Z"),
                       paste0(lubridate::second(intime), ".000Z"))))

  return(outtime)
}

#' terrestrial_core_sites
#'
#' Return a vector listing NEON core terrestrial sites.
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'

terrestrial_core_sites <- function() {

  # core sites as of 190523.

  core_sites <- c("BONA", "CLBJ", "CPER", "GUAN", "HARV", "KONZ",
                  "NIWO", "ONAQ", "ORNL", "OSBS", "PUUM", "SCBI",
                  "SJER", "SRER", "TALL", "TOOL", "UNDE", "WOOD",
                  "WREF", "YELL")

  return(core_sites)
}

#' terrestrial_relocatable_sites
#'
#' Return a vector listing NEON core terrestrial sites.
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}

terrestrial_relocatable_sites <- function() {

  # relocatable sites as of 190523.

  reloc_sites <- c("ABBY", "BARR", "BART", "BLAN", "DCFS", "DEJU",
                   "DELA", "DSNY", "GRSM", "HEAL", "JERC", "JORN",
                   "KONA", "LAJA", "LENO", "MLBS", "MOAB", "NOGP",
                   "OAES", "RMNP", "SERC", "SOAP", "STEI", "STER",
                   "TEAK", "TREE", "UKFS")

  return(reloc_sites)

}

#' water_isotope_sites
#'
#' Return a vector listing NEON sites measuring water vapor isotope ratios.
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}

water_isotope_sites <- function() {

  # water isotope sites as of 200608.

  wiso_sites <- c("BONA", "CLBJ", "CPER", "GUAN", "HARV", "KONZ",
                  "NIWO", "ONAQ", "ORNL", "OSBS", "PUUM", "SCBI",
                  "SJER", "SRER", "TALL", "TOOL", "UNDE", "WOOD",
                  "WREF", "YELL", "BARR")

  return(wiso_sites)

}

#' Title
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param file_dir 
#' @param get 
#' @param trim 
#'
#' @return
#' @export
#'
#' @examples
manage_local_EC_archive <- function(file_dir,
                                    get = TRUE,
                                    trim = FALSE) {
  
  # list the files in file_dir
  files <- list.files(path = file_dir,
                      pattern = "*.h5",
                      recursive = TRUE,
                      full.names = TRUE)
  
  #need to extract sites, months from file names.
  file_pieces <- strsplit(files, split = ".", fixed = TRUE)
  
  #Unless there's a very unusual file path, the 
  #site name should be element 3 of resulting list, 
  #and site year/month should be element 8. We'll
  #also want element 10, which is either 'h5' in the old
  #file naming convention, or is the publication date/time.
  
  sites <- sapply(file_pieces, "[[", 3)
  yrmn  <- sapply(file_pieces, "[[", 8)
  fdiff <- sapply(file_pieces, "[[", 10)
  
  print(head(sites))
  print(head(yrmn))
  print(head(fdiff))

  site_list <- unique(sites)
  
  for (i in seq_along(site_list)) {
    # get list of files where site == site[i]
    isite <- sites == site_list[i]
    
    # check to see if there are duplicates of yrmn
    yrmn_isite <- yrmn[isite]
    
    # test to see if there are any duplicates
    if (sum(duplicated(yrmn_isite) > 0)) {
      
      # get list of duplicated months
      print(paste(site_list[i],yrmn_isite[duplicated(yrmn_isite) | duplicated(yrmn_isite, fromLast = TRUE)]))
    }

    
  }
  
}

