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

#' manage_local_EC_archive
#' 
#' Utility function to help retrieve new EC data and/or prune duplicates,
#' as NEON provisions new data or re-provisions data for an existing site 
#' and month. NOTE: CURRENTLY ONLY THE TRIM FUNCTIONALITY HAS BEEN ADDED.
#' IT IS TURNED OFF BY DEFAULT, AND MUST BE MANUALLY INVOKED WITH trim=TRUE
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param file_dir Specify the root directory where the local EC store is kept.
#' @param get Pull down data from NEON API that does not exist locally?
#' @param trim Search through local holdings, and remove older file where 
#'             there are duplicates?
#'
#' @return
#' @export
#'
#' @examples
manage_local_EC_archive <- function(file_dir,
                                    get = TRUE,
                                    trim = FALSE) {

  if (trim == TRUE) {
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
    
    # print(head(sites))
    # print(head(yrmn))
    # print(head(fdiff))
    
    site_list <- unique(sites)
    
    for (i in seq_along(site_list)) {
      # get list of files where site == site[i]
      isite <- sites == site_list[i]
      
      # check to see if there are duplicates of yrmn
      yrmn_isite <- yrmn[isite]
      fdiff_isite <- fdiff[isite]
      files_isite <- files[isite]
      
      # test to see if there are any duplicates
      if (sum(duplicated(yrmn_isite) > 0)) {
        
        # get list of duplicated months
        #print(paste(site_list[i],yrmn_isite[duplicated(yrmn_isite) | duplicated(yrmn_isite, fromLast = TRUE)]))
        
        # print list of files that are duplicated?
        dups <- duplicated(yrmn_isite) | duplicated(yrmn_isite, fromLast = TRUE)
        
        # narrow list to just list of candidate duplicate files.
        dup_candidates <- files_isite[dups]
        dup_yrmn       <- yrmn_isite[dups]
        dup_fdiff      <- fdiff_isite[dups]
        
        # check to see if one site has an 'h5' fdiff and the duplicates
        # have a date.
        if (!is.null(dup_candidates) & any(fdiff_isite[dups] == 'h5')) {
          h5files <- fdiff_isite[dups] == 'h5'
          print(paste('Removing:',dup_candidates[h5files]))
          file.remove(dup_candidates[h5files]) # remove files.
        } else { # none are simply h5, so need to determine which is the most recent file.
          for (i in 1:length(unique(dup_yrmn))) {
            # get times associated w/ particular duplicate.
            h5_times <- as.POSIXct(dup_fdiff[dup_yrmn == unique(dup_yrmn)[i]], format = "%Y%m%dT%H%M%SZ")
            # determine which files are not the most recent.
            # get file names for only this yrmn.
            dups_yrmn <- dup_candidates[(dup_yrmn == unique(dup_yrmn)[i]) & (h5_times != max(h5_times))]
            # print which files to remove
            print(paste('Removing:',dups_yrmn))
            file.remove(dups_yrmn)
          }
        }
      }
    }
  } 

}

