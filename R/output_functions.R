# output_functions.R
# functions that write data to hdf5 output files.
#################################################
### FUNCTIONS THAT WORK FOR BOTH H2O AND CO2 ####
#################################################
#' setup_output_file
#'
#' Creates a skeleton hdf5 file for the calibrated data.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname Input file name.
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param analyte Carbon ('Co2') or water ('H2o') system?
#'
#' @return Nothing to the environment, but creates a new data file
#'         with the most basic output HDF5 structure consistent with
#'         NEON's data files.
#'
setup_output_file <- function(inname, outname, site, analyte) {

  analyte <- validate_analyte(analyte)

  rhdf5::h5createFile(outname)
  rhdf5::h5createGroup(outname, paste0("/", site))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/iso", analyte))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/iso", analyte))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/iso", analyte))

  rhdf5::h5closeAll()

  # copy attributes from source file and write to output file.
  fid <- rhdf5::H5Fopen(outname)
  tmp <- rhdf5::h5readAttributes(inname[1], paste0("/", site))

  attrloc <- rhdf5::H5Gopen(fid, paste0("/", site))

  for (i in 1:length(tmp)) {
    # probably a more rapid way to do this in the future...lapply?
    rhdf5::h5writeAttribute(h5obj = attrloc,
                            attr = tmp[[i]],
                            name = names(tmp)[i])
  }

  rhdf5::H5Gclose(attrloc)
  rhdf5::h5closeAll()

}


#######################################
### FUNCTIONS THAT WORK ON ONLY CO2 ###
#######################################
#' write_carbon_calibration_data
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param cal_df Calibration data frame -
#'              this is the output from fit_carbon_regression
#' @param method Was the Bowling et al. 2003 or the linear regression
#'          method used in fit_carbon_regression?
#' @param to_file Write to file (TRUE) or to environment (FALSE).
#'
#' @return Nothing to the environment, but writes out the
#'         calibration parameters (e.g., gain and offset or
#'         regression slopes and intercepts) to the output
#'         hdf5 file.
#'
write_carbon_calibration_data <- function(outname,
                                          site,
                                          cal_df,
                                          method,
                                          to_file = TRUE) {

  print("Writing calibration parameters...")
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoCo2/calData"))

  fid <- rhdf5::H5Fopen(outname)
  co2_cal_outloc <- rhdf5::H5Gopen(fid,
                                   paste0("/",
                                          site,
                                          "/dp01/data/isoCo2/calData"))

  if (method == "Bowling_2003") {
    rhdf5::h5writeDataset(obj = cal_df,
                          h5loc = co2_cal_outloc,
                          name = "calGainsOffsets",
                          DataFrameAsCompound = TRUE)
  } else if (method == "linreg") {
    rhdf5::h5writeDataset(obj = cal_df,
                          h5loc = co2_cal_outloc,
                          name = "calRegressions",
                          DataFrameAsCompound = TRUE)
  }

  rhdf5::H5Gclose(co2_cal_outloc)

  # close the group and the file
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()

}

#' write_carbon_ambient_data
#'
#' Write out ambient observations from the NEON EC
#' towers where the isotope data (either H2O or CO2)
#' have been calibrated using this package.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param amb_data_list Calibrated list of ambient data -
#'   this is the output from one of the calibrate_ambient_carbon* functions.
#'
#' @param to_file Write to file (TRUE) or to environment (FALSE).
#'
#' @return Nothing to the environment, but writes data in amb_data_list to file.
#'
write_carbon_ambient_data <- function(outname,
                                      site,
                                      amb_data_list,
                                      to_file = TRUE) {

  print("Writing calibrated ambient data...")

  fid <- rhdf5::H5Fopen(outname)

  if (length(amb_data_list) > 0) {
    for (i in 1:length(amb_data_list)) {
      amb_data_subset <- amb_data_list[i]

      co2_data_outloc <- rhdf5::H5Gcreate(fid,
                                          paste0("/",
                                                 site,
                                                 "/dp01/data/isoCo2/",
                                                 names(amb_data_subset)))

      amb_data_subset <- amb_data_subset[[1]] # list hack

      # loop through variables in amb_data_list and write as a dataframe.
      lapply(names(amb_data_subset),
             function(x) {
               rhdf5::h5writeDataset(obj = amb_data_subset[[x]],
                                     h5loc = co2_data_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)
             })
      rhdf5::H5Gclose(co2_data_outloc)
    }

  }

  # close all open handles.
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()

}

#######################################
### FUNCTIONS THAT WORK ON ONLY H2O ###
#######################################
#' write_water_calibration_data
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param cal_df Calibration data frame -
#'              this is the output from fit_water_regression
#'
#' @return Nothing to the environment, but writes out the
#'         calibration parameters (e.g.,
#'         regression slopes and intercepts) to the output
#'         hdf5 file.
#'
write_water_calibration_data <- function(outname, site, cal_df) {

  print("Writing calibration parameters...")

  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/isoH2o/calData"))

  fid <- rhdf5::H5Fopen(outname)

  h2o_cal_outloc <- rhdf5::H5Gopen(fid,
                                   paste0("/",
                                          site,
                                          "/dp01/data/isoH2o/calData"))

  # write out dataset.
  rhdf5::h5writeDataset(obj = cal_df,
                        h5loc = h2o_cal_outloc,
                        name = "calRegressions",
                        DataFrameAsCompound = TRUE)

  # close the group and the file
  rhdf5::H5Gclose(h2o_cal_outloc)

  # close the group and the file
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()

}

#' write_water_ambient_data
#'
#' Write out ambient observations from the NEON EC
#' towers where the isotope data
#' have been calibrated using this package.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param amb_data_list Calibrated list of ambient data -
#'   this is the output from one of the calibrate_ambient_water* functions.
#'
#' @return Nothing to the environment, but writes data in amb_data_list to file.
#'
write_water_ambient_data <- function(outname, site, amb_data_list) {

  print("Writing calibrated ambient data...")

  fid <- rhdf5::H5Fopen(outname)

  if (length(amb_data_list) > 0) {
    for (i in 1:length(amb_data_list)) {
      amb_data_subset <- amb_data_list[i]

      h2o_data_outloc <- rhdf5::H5Gcreate(fid,
                                          paste0("/",
                                                 site,
                                                 "/dp01/data/isoH2o/",
                                                 names(amb_data_subset)))

      amb_data_subset <- amb_data_subset[[1]] # list hack

      # loop through variables in amb_data_list and write as a dataframe.
      lapply(names(amb_data_subset),
             function(x) {
               rhdf5::h5writeDataset(obj = amb_data_subset[[x]],
                                     h5loc = h2o_data_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)
             })
      rhdf5::H5Gclose(h2o_data_outloc)
    }

  }

  # close all open handles.
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()
}
