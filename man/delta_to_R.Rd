% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isotope_conversions.R
\name{delta_to_R}
\alias{delta_to_R}
\title{delta_to_R}
\usage{
delta_to_R(delta_values, element)
}
\arguments{
\item{delta_values}{A vector of isotope ratios in delta notation.}

\item{element}{Which element to return R values -
carbon, oxygen, or hydrogen.}
}
\value{
Vector of isotope ratios (R values).
}
\description{
delta_to_R
}
\examples{
delta_to_R(delta_values = 0, element = 'oxygen') # 2005.2e-6 for VSMOW.

}
\author{
Rich Fiorella \email{rfiorella@lanl.gov}
}
