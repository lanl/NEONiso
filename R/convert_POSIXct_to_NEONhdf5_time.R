#' convert_POSIXct_to_NEONhdf5_time
#'
#' @param intime Time to convert from NEON HDF5 files. Should be a character string.
#'
#' @return Returns POSIXct version of NEON time variable.
#' @export
#'
#' @examples
#' 
#' 
convert_POSIXct_to_NEONhdf5_time <- function(intime) {
  
  # convert from POSIXct to 
  # a time in YYmmddTHH:MM:SSZ format used by NEON hdf5 files.
  outtime <- as.character(paste0(year(intime),"-",
                                 ifelse(month(intime)<10,paste0("0",month(intime)),month(intime)),"-",
                                 ifelse(day(intime)<10,paste0("0",day(intime)),day(intime)),"T",
                                 ifelse(hour(intime)<10,paste0("0",hour(intime)),hour(intime)),":",
                                 ifelse(minute(intime)<10,paste0("0",minute(intime)),minute(intime)),":",
                                 ifelse(second(intime)<10,paste0("0",second(intime)),second(intime)),"Z"))
  
  return(outtime)
}