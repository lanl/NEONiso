#' Title
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param std.frame Standard data frame to perform swap on.
#' @param dxs.thres d-excess threshold to indicate when to swap.
#'
#' @noRd

swap_standard_isotoperatios <- function(std_frame, dxs_thres = 500) {
  # calculate d excess
  dxs <- std_frame$d2H_ref_mean - 8 * std_frame$d18O_ref_mean

  # if d-excess is extremely positive, indicates a really low d18O value for
  # a given d2H value, and potentially switched isotope ratios in the standard
  inds <- which(dxs > dxs_thres)

  tmpa <- std_frame$d2H_ref_mean
  tmpb <- std_frame$d18O_ref_mean

  if (sum(inds) > 0) {
    # swap isotope ratios where inds = TRUE
    std_frame$d2H_ref_mean[inds] <- tmpb[inds]
    std_frame$d18O_ref_mean[inds] <- tmpa[inds]
  }

  # return fixed dataframe
  return(std_frame)
}
