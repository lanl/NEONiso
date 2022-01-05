# quality control - functions that help to validate the output data file structure

#' validate_output_file
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#' 
#' @param inname Input file name.
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param analyte Carbon ('Co2') or water ('H2o') system?
#'
#' @return Nothing to environment, simply checks to make sure expected groups
#'         are in output.
#'
#' @examples
validate_output_file <- function(inname, outname, site, analyte) {
  
  analyte <- validate_analyte(analyte)
  
  # retrieve groups from input and output files
  groups_in <- rhdf5::h5ls(inname, recursive = 5)
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