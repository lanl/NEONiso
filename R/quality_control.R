# quality control - functions that help to validate the output data file structure

#' validate_analyte
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#' @param analyte Co2 or H2o?
#'
#' @return Standardized string for the water ('H2o') or
#'         carbon ('Co2') systems to make sure strings
#'         are standardized across package functions.
#'
#' 
#' 
validate_analyte <- function(analyte) {
  # helper function to make sure all of the variaous output functions are consistent.
  # check to make sure first letter of analyte is capitalized,
  # or capitalize if it's not (also make sure it's co2 or h2o)
  if (analyte == 'co2' | analyte == 'h2o') {
    analyte <- paste0(toupper(substring(analyte,1,1)),substring(analyte,2))
  } else if (analyte != 'Co2' & analyte != 'H2o') {
    stop("Invalid analyte selected in setup output file.")
  }
  
  return(analyte)
}


#' validate_output_file
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#' 
#' @param inname Input file name.
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param analyte Carbon ('Co2') or water ('H2o') system?
#'
#' @return Nothing to environment, simply checks to make sure expected groups
#'         are in output.
#'
validate_output_file <- function(inname, outname, site, analyte) {
  
  analyte <- validate_analyte(analyte)
  
  # retrieve groups from input and output files
  groups_in <- rhdf5::h5ls(inname[[1]], recursive = 5)
  groups_out <- rhdf5::h5ls(outname, recursive = 5)
  
  if (analyte == 'Co2') {
    target_in <- groups_in[groups_in$group == paste0('/',site,'/dp01/data/isoCo2'),]$name
    target_in <- target_in[grepl('09m', target_in) &
                          !grepl('Arch', target_in)] #only care about the 9m vars!
    target_out <- groups_out[groups_out$group == paste0('/', site, '/dp01/data/isoCo2'),]$name
    
  } else {
    target_in <- groups_in[groups_in$group == paste0('/',site,'/dp01/data/isoH2o'),]$name
    target_in <- target_in[grep('09m', target_in) &
                          !grepl('Arch', target_in)] #only care about the 9m vars!
    target_out <- groups_out[groups_out$group == paste0('/', site, '/dp01/data/isoH2o'),]$name
  }
  
  # add calData to target_in, since we expect this to be added (and adding to
  # target_in confirms that it remains in target_out!)
  target_in <- c(target_in, "calData")

  # order both vectors
  target_in  <- sort(target_in)
  target_out <- sort(target_out)
  
  # check to see if equal
  test_result <- identical(target_in, target_out)
  
  # throw error (warning?) if not identical
  if (!test_result) {
    print(c(target_in, target_out))
    stop("Output file structure does not contain expected groups, has diverged from input file!")
  }
}
