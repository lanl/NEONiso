% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restructure_data.R
\name{restructure_water_variables}
\alias{restructure_water_variables}
\title{restructure_water_variables}
\usage{
restructure_water_variables(dataframe, varname, mode, group)
}
\arguments{
\item{dataframe}{Input data.frame, from \code{neonUtilities::stackEddy}}

\item{varname}{Which variable are we applying this function to? There's
a list of ~10 common ones to write to the hdf5 file.}

\item{mode}{Are we fixing a reference data frame or an ambient data frame?}

\item{group}{Data, ucrt, or qfqm?}
}
\value{
data.frame formatted for output to hdf5 file.
}
\description{
restructure_water_variables
}
