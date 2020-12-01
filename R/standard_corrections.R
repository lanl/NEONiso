#' swap_standard_isotoperatios
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

#' correct_carbon_ref_cval
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param std_frame Standard data frame to perform swap on.
#' @param site NEON four letter site code.
#'
#' @noRd

correct_carbon_ref_cval <- function(std_frame,site) {

  # updates 201125 - there are in fact a few places where
  # cval data seem to be different than ci data in current hdf5 files
  sites_with_corrections <- c("ONAQ","WOOD","BLAN","ORNL","TREE","STER","BARR","SRER")
  
  if (!(site %in% sites_with_corrections)) {
    # nothing to do.
    return(std_frame)
    
  } else {
    
    # implementing the change here:
    if (site == "ONAQ") {
      print("Correcting ONAQ reference values between 6/18/18 and 2/7/19...")
      # all reference gases have incorrect d13C and CO2 mol frac
      # between 6/18/18 and 2/7/2019
      
      corr_start <- as.POSIXct("06/18/2018", format = "%m/%d/%Y")
      corr_end   <- as.POSIXct("02/07/2019", format = "%m/%d/%Y")
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "high" &
                               (std_frame$CO2_ref_mean > 465)] <- 466.643
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "high" &
                                (std_frame$d13C_ref_mean < -11)] <- -10.539
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "med" &
                               (std_frame$CO2_ref_mean > 450)] <- 412.103
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "med" &
                                (std_frame$d13C_ref_mean < -10)] <- -9.102
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "low" &
                               (std_frame$CO2_ref_mean > 415)] <- 366.939
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "low" &
                                (std_frame$d13C_ref_mean < -9.2)] <- -9.054
      
    } else if (site == "WOOD") {
      
      corr_start <- as.POSIXct("06/18/2018", format = "%m/%d/%Y")
      corr_end   <- as.POSIXct("02/07/2019", format = "%m/%d/%Y")
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "low" &
                               (std_frame$CO2_ref_mean > 363 & std_frame$CO2_ref_mean < 364)] <- 366.56
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "low" &
                                (std_frame$d13C_ref_mean < -8.5 & std_frame$d13C_ref_mean < -9)] <- -9.267
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "med" &
                               (std_frame$CO2_ref_mean > 450 & std_frame$CO2_ref_mean < 461)] <- 412.503
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "med" &
                                (std_frame$d13C_ref_mean < -10.4 & std_frame$d13C_ref_mean > -10.6)] <- -9.448
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "high" &
                               (std_frame$CO2_ref_mean > 560 & std_frame$CO2_ref_mean < 570)] <- 454.619
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "high" &
                                (std_frame$d13C_ref_mean < -11.4 & std_frame$d13C_ref_mean > -11.8)] <- -10.392
      
    } else if (site == "BLAN") {
      
      corr_start <- as.POSIXct("04/01/2019", format = "%m/%d/%Y")
      corr_end   <- as.POSIXct("08/22/2019", format = "%m/%d/%Y")
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "low" & (is.na(std_frame$CO2_ref_mean) | 
                              (std_frame$CO2_ref_mean > 360 & std_frame$CO2_ref_mean < 362))] <- 362.54
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "low" & (is.na(std_frame$d13C_ref_mean) | 
                              (std_frame$d13C_ref_mean < -8.3 & std_frame$d13C_ref_mean > -8.6))] <- -8.955
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "med" & (is.na(std_frame$CO2_ref_mean) | 
                              (std_frame$CO2_ref_mean > 410 & std_frame$CO2_ref_mean < 415))] <- 435.80
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "med" & (is.na(std_frame$d13C_ref_mean) |
                               (std_frame$d13C_ref_mean < -9.4 & std_frame$d13C_ref_mean > -9.6))] <- -10.116
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "high" & (is.na(std_frame$CO2_ref_mean) |
                              (std_frame$CO2_ref_mean > 505 & std_frame$CO2_ref_mean < 515))] <- 521.08
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "high" & (is.na(std_frame$d13C_ref_mean) |
                               (std_frame$d13C_ref_mean < -10.5 & std_frame$d13C_ref_mean > -11.0))] <- -15.418
      
    } else if (site == "STER") {
      
      corr_start <- as.POSIXct("01/16/2019", format = "%m/%d/%Y")
      corr_end   <- as.POSIXct("05/08/2019", format = "%m/%d/%Y")
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "low" & (is.na(std_frame$CO2_ref_mean) | 
                              (std_frame$CO2_ref_mean > 350 & std_frame$CO2_ref_mean < 352))] <- 360.047
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "low" & (is.na(std_frame$d13C_ref_mean) | 
                               (std_frame$d13C_ref_mean < -8.4 & std_frame$d13C_ref_mean > -8.6))] <- -8.763
      
    } else if (site == "ORNL") {
      
      # ORNL is a mess...different wrong dates for the high/med standards than for the low standard.
      corr_start <- as.POSIXct("01/01/2020", format = "%m/%d/%Y")
      corr_end   <- as.POSIXct("05/31/2020", format = "%m/%d/%Y")
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "low" & 
                              (std_frame$CO2_ref_mean > 350 & std_frame$CO2_ref_mean < 352)] <- 367.82
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "low" & 
                              (std_frame$d13C_ref_mean < -8.5 & std_frame$d13C_ref_mean > -8.7)] <- -9.049
      
      # set correct start time for med/high.
      corr_start <- as.POSIXct("11/14/2019", format = "%m/%d/%Y")
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "med" &  
                              (std_frame$CO2_ref_mean > 450 | std_frame$CO2_ref_mean < 425)] <- 442.565
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "med" & 
                              (std_frame$d13C_ref_mean > -10.5 | std_frame$d13C_ref_mean < -10.6)] <- -10.575
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "high" &
                              std_frame$CO2_ref_mean < 530] <- 530.234
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "high" &
                              std_frame$d13C_ref_mean > -15.0] <- -15.575
      
    } else if (site == "TREE") {
      
      corr_start <- as.POSIXct("01/01/2020", format = "%m/%d/%Y")
      corr_end   <- as.POSIXct("05/31/2020", format = "%m/%d/%Y")
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "high" &
                               (std_frame$CO2_ref_mean > 520)] <- 511.21
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "high" &
                                (std_frame$d13C_ref_mean < -16)] <- -14.828
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "med" &
                               (std_frame$CO2_ref_mean < 400)] <- 437.92
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "med" &
                                (std_frame$d13C_ref_mean > -10)] <- -10.284
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "low" &
                               (std_frame$CO2_ref_mean < 350)] <- 368.58
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "low" &
                                (std_frame$d13C_ref_mean > -9)] <- -9.175
      
    } else if (site == "BARR") {
      
      corr_start <- as.POSIXct("06/01/2018", format = "%m/%d/%Y")
      corr_end   <- as.POSIXct("03/28/2019", format = "%m/%d/%Y")
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "high" &
                               (std_frame$CO2_ref_mean > 420 & std_frame$CO2_ref_mean < 430)] <- 555.26
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "high" &
                                (std_frame$d13C_ref_mean < -9.9 & std_frame$d13C_ref_mean > -10)] <- -11.501
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "low" &
                               (std_frame$CO2_ref_mean < 390 & std_frame$CO2_ref_mean > 380)] <- 359.05
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "low" &
                                (std_frame$d13C_ref_mean > -8.9 & std_frame$d13C_ref_mean < -8.8)] <- -8.607
      
    } else if (site == "SRER") {
      
      corr_start <- as.POSIXct("04/01/2018", format = "%m/%d/%Y")
      corr_end   <- as.POSIXct("08/01/2018", format = "%m/%d/%Y")
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "low" & 
                               (std_frame$CO2_ref_mean > 380 | is.na(std_frame$CO2_ref_mean))] <- 356.166
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "low" & 
                                (std_frame$d13C_ref_mean < -9 | is.na(std_frame$d13C_ref_mean))] <- -8.702
      
      std_frame$CO2_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                std_frame$d13C_ref_btime < corr_end) &
                               std_frame$std_name == "med" & 
                               (std_frame$CO2_ref_mean > 485 | is.na(std_frame$CO2_ref_mean))] <- 428.784
      std_frame$d13C_ref_mean[(std_frame$d13C_ref_btime > corr_start &
                                 std_frame$d13C_ref_btime < corr_end) &
                                std_frame$std_name == "med" & 
                                (std_frame$d13C_ref_mean < -14 | is.na(std_frame$d13C_ref_mean))] <- -10.401
      
    }
    
    
  }
  # return fixed dataframe
  return(std_frame)
}

