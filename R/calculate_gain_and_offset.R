#' calculate_gain_and_offset.R
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#' 
#' @param std1 First standard used in Bowling et al. gain and offset calibration.
#' @param std2 Second standard used in Bowling et al. gain and offset calibration.
#'
#' @return Data frame of gain and offset values, along with variance estimates, for 12CO2 and 13CO2.
#' @export
#'
#' @examples
calculate_gain_and_offset <- function(std1,std2) {

  # calculate gain
  gain12C <- (std1$conc12CCO2_ref - std2$conc12CCO2_ref)/(std1$conc12CCO2_obs - std2$conc12CCO2_obs)
  gain13C <- (std1$conc13CCO2_ref - std2$conc13CCO2_ref)/(std1$conc13CCO2_obs - std2$conc13CCO2_obs)

  # calculate offset
  offset12C <- std1$conc12CCO2_ref - gain12C*std1$conc12CCO2_obs
  offset13C <- std1$conc13CCO2_ref - gain13C*std1$conc13CCO2_obs
  
  # calculate uncertainty on gain values.
  vari.g12C <- (std1$vari12CCO2_ref + std2$vari12CCO2_ref)/ # already variance, so already sigma^2
    (std1$conc12CCO2_obs - std2$conc12CCO2_obs)^2 +
    (std1$vari12CCO2_obs + std2$vari12CCO2_obs)* # already variance, so already sigma^2
    ((std1$conc12CCO2_ref - std2$conc12CCO2_ref)/(std1$conc12CCO2_obs - std2$conc12CCO2_obs)^2)^2
  
  vari.g13C <- (std1$vari13CCO2_ref + std2$vari13CCO2_ref)/ # already variance, so already sigma^2
    (std1$conc13CCO2_obs - std2$conc13CCO2_obs)^2 +
    (std1$vari13CCO2_obs + std2$vari13CCO2_obs)* # already variance, so already sigma^2
    ((std1$conc13CCO2_ref - std2$conc13CCO2_ref)/(std1$conc13CCO2_obs - std2$conc13CCO2_obs)^2)^2
  
  vari.o12C <- std1$vari12CCO2_ref + std1$conc12CCO2_obs^2*vari.g12C + 
    gain12C^2*std1$vari12CCO2_obs
  
  vari.o13C <- std1$vari13CCO2_ref + std1$conc13CCO2_obs^2*vari.g13C + 
    gain13C^2*std1$vari13CCO2_obs
  
  # return a list of gain and uncertainty values.
  return(data.frame(gain12C,vari.g12C,gain13C,vari.g13C,offset12C,vari.o12C,offset13C,vari.o13C))
  
}