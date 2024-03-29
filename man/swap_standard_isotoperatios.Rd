% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standard_corrections.R
\name{swap_standard_isotoperatios}
\alias{swap_standard_isotoperatios}
\title{swap_standard_isotoperatios}
\usage{
swap_standard_isotoperatios(std_frame, dxs_thres = 500)
}
\arguments{
\item{std_frame}{Standard data frame to perform swap on.}

\item{dxs_thres}{d-excess threshold to indicate when to swap.}
}
\value{
A data.frame based on \code{std_frame}, where d18O and
d2H values have been swapped from NEON input files if
determined to have a reference value mismatch. Mismatch
is determined based on the d-excess of the standard (=
d2H - 8*d18O), using a value of 500 by default.
}
\description{
There are a few suspected instances where the water
isotope ratios for oxygen and hydrogen have been flipped
in the reference data. This function corrects them until
they are corrected in the NEON database using a d-excess
filter.
}
\author{
Rich Fiorella \email{rfiorella@lanl.gov}
}
