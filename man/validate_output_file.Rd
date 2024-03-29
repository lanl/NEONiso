% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_control.R
\name{validate_output_file}
\alias{validate_output_file}
\title{validate_output_file}
\usage{
validate_output_file(inname, outname, site, analyte)
}
\arguments{
\item{inname}{Input file name.}

\item{outname}{Output file name.}

\item{site}{NEON 4-letter site code.}

\item{analyte}{Carbon ('Co2') or water ('H2o') system?}
}
\value{
Nothing to environment, simply checks to make sure expected groups
are in output.
}
\description{
validate_output_file
}
\author{
Rich Fiorella \email{rfiorella@lanl.gov}
}
