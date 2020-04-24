#' calculate_gain_and_offset.R
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param std1 First standard used in Bowling et al.
#'             gain and offset calibration.
#' @param std2 Second standard used in Bowling et al.
#'            gain and offset calibration.
#'
#' @return Data frame of gain and offset values for 12CO2 and 13CO2.
#' @export
#'
#' @examples
calculate_gain_and_offset <- function(std1, std2) {

  # calculate gain
  gain12c <- (std1$conc12CCO2_ref - std2$conc12CCO2_ref) /
    (std1$conc12CCO2_obs - std2$conc12CCO2_obs)

  gain13c <- (std1$conc13CCO2_ref - std2$conc13CCO2_ref) /
    (std1$conc13CCO2_obs - std2$conc13CCO2_obs)

  # calculate offset
  offset12c <- std1$conc12CCO2_ref - gain12c * std1$conc12CCO2_obs
  offset13c <- std1$conc13CCO2_ref - gain13c * std1$conc13CCO2_obs

  # return a list of gain and uncertainty values.
  return(data.frame(gain12c, gain13c, offset12c, offset13c))

}