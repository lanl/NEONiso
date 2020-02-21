# filtering_functions.R
# a file with a few optinos for filtering output data.



#---------------------------------
#' Title
#'
#' @param data 
#' @param width 
#' @param threshold 
#'
#' @return
#' @export
#'
#' @examples
filter_median_Brock86 <- function(data,width=7,threshold=5) {
  
  # get rolling median of data.
  filt <- zoo::rollapply(data,width,median,na.rm=TRUE,fill=NA)
  
  # get logical vector of what indices are valid.
  spikes <- abs(data - filt) > threshold
  
  # set spikes as missing - consider interpolation in future release?
  data[data == TRUE] <- NA
  
  # return data vector back
  return(data)
}