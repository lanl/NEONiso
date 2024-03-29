% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reference_data_extraction.R
\name{extract_carbon_cal_data}
\alias{extract_carbon_cal_data}
\title{extract_carbon_calibration_data.R}
\usage{
extract_carbon_cal_data(
  data_list,
  standards = c("co2Low", "co2Med", "co2High")
)
}
\arguments{
\item{data_list}{List containing data, from the /*/dp01/data/
group in NEON HDF5 file.}

\item{standards}{Which reference gases (standards) to use? Default is all,
but can pass a subset of "co2Low", "co2Med", and "co2High" as a vector
to this argument as well.}
}
\value{
Returns data frame of required variables.
}
\description{
extract_carbon_calibration_data.R
}
\author{
Rich Fiorella \email{rfiorella@lanl.gov}
}
