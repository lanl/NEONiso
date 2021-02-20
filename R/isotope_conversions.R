# isotope functions
# break out functions that are various transformations
# of isotope ratios and delta values.
#------------------------------------------------------
#' Title
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#' 
#' @param element 
#'
#' @return
#' @export
#'
#' @examples
#' 
get_Rstd <- function(element) {
  # return the standard isotope ratio
  if (element == "carbon") {
    R <- 0.0111797 # 13C/12C ratio for VPBD standard.
  } else if (element == "oxygen") {
    R <- 2005.20e-6 # SMOW 18O/16O ratio
  } else if (element == "hydrogen") {
    R <- 155.76e-6 # SMOW 2H/1H ratio
  }
  return(R)
}

#' Title
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data_vector 
#' @param element 
#'
#' @return
#' @export
#'
#' @examples
R_to_delta <- function(data_vector,element) {
  
  # get standard isotope ratio
  Rstd <- get_Rstd(element)
  
  #convert R to delta
  delta <- 1000*(data_vector/Rstd - 1)
  
  # return delta values
  return(delta)
}

#' Title
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data_vector 
#' @param element 
#'
#' @return
#' @export
#'
#' @examples
delta_to_R <- function(data_vector,element) {
  
  # get standard isotope ratio
  Rstd <- get_Rstd(element)
  #convert R to delta
  R <- Rstd*(data_vector/1000 + 1)
  # return delta values
  return(R)
  
}
