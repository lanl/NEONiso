# isotope functions
# break out functions that are various transformations
# of isotope ratios and delta values.
#------------------------------------------------------
#' get_Rstd
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#' 
#' @param element Which element to return standard ratio - 
#'        carbon, oxygen, or hydrogen.
#'
#' @return Heavy-to-light isotope ratio of most common
#'         stable isotope standard. VMOW for water,
#'         VPDB for carbon.
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

#' R_to_delta
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data_vector A vector of isotope ratios (e.g., R values).
#' @param element Which element to return delta values - 
#'        carbon, oxygen, or hydrogen.
#'
#' @return Vector of isotope ratios in delta notation.
#'
R_to_delta <- function(data_vector,element) {
  
  # get standard isotope ratio
  Rstd <- get_Rstd(element)
  
  #convert R to delta
  delta <- 1000*(data_vector/Rstd - 1)
  
  # return delta values
  return(delta)
}

#' delta_to_R
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data_vector A vector of isotope ratios in delta notation.
#' @param element Which element to return R values - 
#'        carbon, oxygen, or hydrogen.
#'
#' @return Vector of isotope ratios (R values).
delta_to_R <- function(data_vector,element) {
  
  # get standard isotope ratio
  Rstd <- get_Rstd(element)
  #convert R to delta
  R <- Rstd*(data_vector/1000 + 1)
  # return delta values
  return(R)
  
}

#' calculate_12CO2
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param total_co2 Vector of CO2 mole fractions.
#' @param delta13C Vector of d13C values.
#' @param f Fraction of CO2 that is not 12CO2 or 13CO2. Assumed fixed
#'          at 0.00474
#'
#' @return Vector of 12CO2 mole fractions.
#'
calculate_12CO2 <- function(total_co2,delta13C,f = 0.00474) {
  
  # convert delta13C to R13
  R <- delta_to_R(delta13C, "carbon")
  
  # calculate 12CO2 from total CO2 and R
  light_co2 <- total_co2 * (1 - f) / (1 + R)
  
  # return 12co2
  return(light_co2)
}

#' calculate_13CO2
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param total_co2 Vector of CO2 mole fractions.
#' @param delta13C Vector of d13C values.
#' @param f Fraction of CO2 that is not 12CO2 or 13CO2. Assumed fixed
#'          at 0.00474
#'          
#' @return Vector of 13CO2 mole fractions.
#'
calculate_13CO2 <- function(total_co2,delta13C,f = 0.00474) {
  
  #convert delta13C to R13
  R <- delta_to_R(delta13C, "carbon")
  
  # calculate 13CO2 from total CO2 and R
  light_co2 <- calculate_12CO2(total_co2, delta13C)
  
  heavy_co2 <- total_co2 * (1-f) - light_co2
  
  # return 13co2
  return(heavy_co2)
  
}
