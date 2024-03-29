% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate_ambient_carbon_Bowling2003.R
\name{calibrate_ambient_carbon_Bowling2003}
\alias{calibrate_ambient_carbon_Bowling2003}
\title{calibrate_ambient_carbon_Bowling2003}
\usage{
calibrate_ambient_carbon_Bowling2003(
  amb_data_list,
  caldf,
  site,
  filter_data = TRUE,
  force_to_end = TRUE,
  force_to_beginning = TRUE,
  gap_fill_parameters = FALSE,
  r2_thres = 0.9
)
}
\arguments{
\item{amb_data_list}{List containing an ambient d13C dataset.
Will include all variables in 000_0x0_xxm. (character)}

\item{caldf}{Calibration data frame containing gain and offset values for
12C and 13C isotopologues.}

\item{site}{Four-letter NEON code corresponding to site being processed.}

\item{filter_data}{Apply median absolute deviation filter from Brock 86 to
remove impulse spikes? Inherited from
\code{calibrate_ambient_carbon_Bowling2003()}}

\item{force_to_end}{In given month, calibrate ambient data later than last
calibration, using the last calibration? (default true)}

\item{force_to_beginning}{In given month, calibrate ambient data before than
first calibration, using the first calibration? (default true)}

\item{gap_fill_parameters}{Should function attempt to 'gap-fill' across a
bad calibration by carrying the last known good calibration
forward? Implementation is fairly primitive currently, as
it only carries the last known good calibration that's
available forward rather than interpolating, etc. Default FALSE.}

\item{r2_thres}{Minimum r2 value for calibration to be considered "good" and
applied to ambient data.}
}
\value{
Depends on \code{write_to_file} argument.
If true, returns nothing to environment;
but returns calibrated ambient observations to the output file.
If false, returns modified version of amb_data_list that include
calibrated ambient data.
}
\description{
calibrate_ambient_carbon_Bowling2003
}
\author{
Rich Fiorella \email{rfiorella@lanl.gov}

Function called by \code{calibrate_carbon_bymonth()} to apply
gain and offset parameters to the ambient datasets (000_0x0_09m and
000_0x0_30m). This function should generally not be used independently,
but should be used in coordination with
\code{calibrate_carbon_bymonth()}.
}
