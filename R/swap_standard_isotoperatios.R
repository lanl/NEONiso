#' Title
#'
#' @param std.frame 
#' @param dxs.thres 
#'
#' @noRd

swap_standard_isotoperatios <- function(std.frame,dxs.thres = 500) {
  # calculate d excess
  dxs <- std.frame$d2H_ref_mean - 8*std.frame$d18O_ref_mean
  
  # if d-excess is extremely positive, indicates a really low d18O value for
  # a given d2H value, and potentially switched isotope ratios in the standard.
  inds = which(dxs > dxs.thres)
  
  tmpa <- std.frame$d2H_ref_mean
  tmpb <- std.frame$d18O_ref_mean
  
  if (sum(inds) > 0) {
    # swap isotope ratios where inds = TRUE
    std.frame$d2H_ref_mean[inds] <- tmpb[inds]
    std.frame$d18O_ref_mean[inds] <- tmpa[inds]
  }
  
  # return fixed dataframe
  return(std.frame)
}