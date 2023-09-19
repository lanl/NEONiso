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

#' write_qfqm
#'
#' Write NEON's qfqm data for an isotope species to
#' output file. Wraps copy_qfqm_group.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname Input file name.
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param analyte Carbon ('Co2') or water ('H2o') system?
#'
#' @return Nothing to the environment, but writes qfqm data to file.
#'
write_qfqm <- function(inname, outname, site, analyte) {

  analyte <- validate_analyte(analyte)

  print("Copying qfqm...")
  # copy over ucrt and qfqm groups as well.
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/iso", analyte))
  qfqm <- rhdf5::h5read(inname, paste0("/", site, "/dp01/qfqm/iso", analyte))

  lapply(names(qfqm), function(x) {
    copy_qfqm_group(data_list = qfqm[[x]],
                    outname = x,
                    file = outname,
                    site = site,
                    species = analyte)})

  rhdf5::h5closeAll()

}

#' write_ucrt
#'
#' Write NEON's ucrt data for an isotope species to
#' output file. Wraps copy_ucrt_group.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname Input file name.
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param analyte Carbon ('Co2') or water ('H2o') system?
#'
#' @return Nothing to the environment, but writes ucrt data to file.
#'
write_ucrt <- function(inname, outname, site, analyte) {

  analyte <- validate_analyte(analyte)

  print("Copying ucrt...")
  # copy over ucrt and qfqm groups as well.
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/iso", analyte))
  ucrt <- rhdf5::h5read(inname, paste0("/", site, "/dp01/ucrt/iso", analyte))

  lapply(names(ucrt), function(x) {
    copy_ucrt_group(data_list = ucrt[[x]],
                    outname = x,
                    file = outname,
                    site = site,
                    species = analyte)})

  rhdf5::h5closeAll()

}

#' copy_qfqm_group
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param data_list List of groups to retrieve qfqm data from.
#' @param outname Output filename.
#' @param site Four-letter NEON site code.
#' @param file Input filename.
#' @param species CO2 or H2O? Same function used for both CO2 and H2O isotopes.
#'
#' @return Nothing to the workspace, but copies qfqm group from input file to
#'         output file.
#'
copy_qfqm_group <- function(data_list, outname, site, file, species) {

  # create hdf5 structure for these variables.
  fid <- rhdf5::H5Fopen(file)

  if (species == "Co2") {
    co2_data_outloc <- rhdf5::H5Gcreate(fid,
                              paste0("/", site, "/dp01/qfqm/isoCo2/", outname))

    # loop through each of the variables in list amb.data.list
    # and write out as a dataframe.
    lapply(names(data_list), function(x) {
      rhdf5::h5writeDataset(obj = data_list[[x]],
                                       h5loc = co2_data_outloc,
                                       name = x,
                                       DataFrameAsCompound = TRUE)})

  } else if (species == "H2o") {

    h2o_data_outloc <- rhdf5::H5Gcreate(fid,
                              paste0("/", site, "/dp01/qfqm/isoH2o/", outname))

    # loop through each of the variables in list amb.data.list
    # and write out as a dataframe.
    lapply(names(data_list), function(x) {
      rhdf5::h5writeDataset(obj = data_list[[x]],
                                       h5loc = h2o_data_outloc,
                                       name = x,
                                       DataFrameAsCompound = TRUE)})

  }

  # close all open handles.
  rhdf5::h5closeAll()

}

#' copy_ucrt_group
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param file Input file name.
#' @param data_list List of groups to retrieve ucrt data from.
#' @param species H2O or CO2.
#'
#' @return Nothing to the workspace, but copies ucrt group from input file to
#'         output file.
#'
copy_ucrt_group <- function(data_list, outname, site, file, species) {

  # create hdf5 structure for these variables.
  fid <- rhdf5::H5Fopen(file)

  if (species == "Co2") {

    co2_data_outloc <- rhdf5::H5Gcreate(fid,
                                paste0("/",
                                       site,
                                       "/dp01/ucrt/isoCo2/",
                                       outname))

    # loop through each variable in amb.data.list and write out as a dataframe
    lapply(names(data_list), function(x) {
      rhdf5::h5writeDataset(obj = data_list[[x]],
                                       h5loc = co2_data_outloc,
                                       name = x,
                                       DataFrameAsCompound = TRUE)})

  } else if (species == "H2o") {

    h2o_data_outloc <- rhdf5::H5Gcreate(fid,
                                paste0("/",
                                       site,
                                       "/dp01/ucrt/isoH2o/",
                                       outname))

    # loop through each variable in amb.data.list and write out as a dataframe.
    lapply(names(data_list), function(x) {
      rhdf5::h5writeDataset(obj = data_list[[x]],
                                       h5loc = h2o_data_outloc,
                                       name = x,
                                       DataFrameAsCompound = TRUE)})

  }

  # close all open handles.
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
#' @param calDf Calibration data frame -
#'              this is the output from fit_carbon_regression
#' @param method Was the Bowling et al. 2003 or the linear regression
#'          method used in fit_carbon_regression?
#'
#' @return Nothing to the environment, but writes out the
#'         calibration parameters (e.g., gain and offset or
#'         regression slopes and intercepts) to the output
#'         hdf5 file.
#'
write_carbon_calibration_data <- function(outname, site, calDf, method) {

  print("Writing calibration parameters...")
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoCo2/calData"))

  fid <- rhdf5::H5Fopen(outname)
  co2_cal_outloc <- rhdf5::H5Gopen(fid,
                          paste0("/", site, "/dp01/data/isoCo2/calData"))

  if (method == "Bowling_2003") {
    rhdf5::h5writeDataset(obj = calDf,
                                     h5loc = co2_cal_outloc,
                                     name = "calGainsOffsets",
                                     DataFrameAsCompound = TRUE)
  } else if (method == "linreg") {
    rhdf5::h5writeDataset(obj = calDf,
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
#' @return Nothing to the environment, but writes data in amb_data_list to file.
#'
write_carbon_ambient_data <- function(outname, site, amb_data_list) {

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
      lapply(names(amb_data_subset), function(x) {
        rhdf5::h5writeDataset(obj = amb_data_subset[[x]],
                                         h5loc = co2_data_outloc,
                                         name = x,
                                         DataFrameAsCompound = TRUE)})
      rhdf5::H5Gclose(co2_data_outloc)
    }

  }

  # close all open handles.
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()

}

#' write_carbon_reference_data
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname Input file name.
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param calDf Calibration data frame -
#'              this is the output from fit_carbon_regression
#'
#' @return Nothing to the environment, but writes calibrated
#'         reference data to hdf5 file.
#'
write_carbon_reference_data <- function(inname, outname, site, calDf) {

  print("Writing calibrated reference data...")
  calibrate_carbon_reference_data(inname, outname, "Low", site, calDf)
  calibrate_carbon_reference_data(inname, outname, "Med", site, calDf)
  calibrate_carbon_reference_data(inname, outname, "High", site, calDf)

}

#' calibrate_carbon_reference_data
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname Input file name.
#' @param outname Output file name.
#' @param standard Which standard are we working on? Must be "Low",
#'                 "Med", or "High"
#' @param site NEON 4-letter site code.
#' @param calDf Calibration data frame -
#'              this is the output from fit_carbon_regression
#'
#' @return Nothing to the environment.
#'
calibrate_carbon_reference_data <- function(inname, outname,
                                            standard, site, calDf)  {

  std <- rhdf5::h5read(inname,
                paste0("/", site, "/dp01/data/isoCo2/co2", standard, "_09m"))

  std <- calibrate_standards_carbon(calDf, std, correct_bad_refvals = TRUE,
                                    site = site, refGas = standard)


  fid <- rhdf5::H5Fopen(outname)
  rhdf5::H5Gcreate(fid,
                   paste0("/", site, "/dp01/data/isoCo2/co2", standard, "_09m"))
  std_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/",
                                      site,
                                      "/dp01/data/isoCo2/co2",
                                      standard,
                                      "_09m"))
  # loop through each variable amb.data.list and write out as a dataframe.
  lapply(names(std), function(x) {
    rhdf5::h5writeDataset(obj = std[[x]],
                                     h5loc = std_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})

  rhdf5::H5Gclose(std_outloc)
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
#' @param calDf Calibration data frame -
#'              this is the output from fit_water_regression
#'
#' @return Nothing to the environment, but writes out the
#'         calibration parameters (e.g.,
#'         regression slopes and intercepts) to the output
#'         hdf5 file.
#'
write_water_calibration_data <- function(outname, site, calDf) {

  print("Writing calibration parameters...")

  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/isoH2o/calData"))

  fid <- rhdf5::H5Fopen(outname)

  h2o_cal_outloc <- rhdf5::H5Gopen(fid,
                              paste0("/", site, "/dp01/data/isoH2o/calData"))

  # write out dataset.
  rhdf5::h5writeDataset(obj = calDf,
                                   h5loc = h2o_cal_outloc,
                                   name = "calRegressions",
                                   DataFrameAsCompound = TRUE)

  # close the group and the file
  rhdf5::H5Gclose(h2o_cal_outloc)

  # close the group and the file
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()

}

#' write_water_reference_data
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname Input file name.
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param calDf Calibration data frame -
#'              this is the output from fit_water_regression
#' @param lowDf Dataframe corresponding to the "low" reference water.
#' @param medDf Data frame corresponding to the "med" reference water.
#' @param highDf Data frame corresponding to the "high" reference water.
#'
#' @return Nothing to the environment, but writes calibrated
#'         reference data to hdf5 file.
#'
write_water_reference_data <- function(inname, outname, site,
                                        lowDf, medDf, highDf, calDf) {

  print("Writing calibrated reference data...")
  calibrate_water_reference_data(outname, "Low", site, lowDf, calDf)
  calibrate_water_reference_data(outname, "Med", site, medDf, calDf)
  calibrate_water_reference_data(outname, "High", site, highDf, calDf)

}

#' calibrate_water_reference_data
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param calDf Calibration data frame -
#'              this is the output from fit_water_regression
#' @param standard Which reference material is being 'calibrated'?
#'                 (Low, med, or high)
#' @param stdDf Data frame of reference material measurements.
#'
#' @return Nothing to the environment.
#'
# - problem here: in some contexts standard is a df,
# others its a string (e.g., which standard?)
calibrate_water_reference_data <- function(outname,
                                           standard,
                                           site,
                                           stdDf,
                                           calDf) {

  rhdf5::h5createGroup(outname,
                       paste0("/", site,
                              "/dp01/data/isoH2o/h2o",
                              standard, "_03m"))

  fid <- rhdf5::H5Fopen(outname)
  std_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site,
                                      "/dp01/data/isoH2o/h2o",
                                      standard, "_03m"))

  # restructure variables to be more suitable for output file.
  dlta18OH2o           <- restructure_water_variables(stdDf,
                                                      "dlta18OH2o",
                                                      "reference")
  dlta2HH2o            <- restructure_water_variables(stdDf,
                                                      "dlta2HH2o",
                                                      "reference")
  dlta18OH2oRefe       <- restructure_water_variables(stdDf,
                                                      "dlta18OH2oRefe",
                                                      "reference")
  dlta2HH2oRefe        <- restructure_water_variables(stdDf,
                                                      "dlta2HH2oRefe",
                                                      "reference")
  pres                 <- restructure_water_variables(stdDf,
                                                      "pres",
                                                      "reference")
  presEnvHut           <- restructure_water_variables(stdDf,
                                                      "presEnvHut",
                                                      "reference")
  rhEnvHut             <- restructure_water_variables(stdDf,
                                                      "rhEnvHut",
                                                      "reference")
  rtioMoleWetH2o       <- restructure_water_variables(stdDf,
                                                      "rtioMoleWetH2o",
                                                      "reference")
  rtioMoleWetH2oEnvHut <- restructure_water_variables(stdDf,
                                                      "rtioMoleWetH2oEnvHut",
                                                      "reference")
  temp                 <- restructure_water_variables(stdDf,
                                                      "temp",
                                                      "reference")
  tempEnvHut           <- restructure_water_variables(stdDf,
                                                      "tempEnvHut",
                                                      "reference")

  data_out_all <- do.call(rbind, list(dlta18OH2o[[1]], dlta2HH2o[[1]],
                                     dlta18OH2oRefe[[1]], dlta2HH2oRefe[[1]],
                                     pres[[1]], presEnvHut[[1]], rhEnvHut[[1]],
                                     rtioMoleWetH2o[[1]],
                                     rtioMoleWetH2oEnvHut[[1]], temp[[1]],
                                     tempEnvHut[[1]]))

  std <- base::split(data_out_all, factor(data_out_all$varname))

  std <- calibrate_standards_water(calDf, std)

  # and write out as a dataframe.
  lapply(names(std), function(x) {
    rhdf5::h5writeDataset(obj = std[[x]],
                                     h5loc = std_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})

  rhdf5::H5Gclose(std_outloc)

  # write qfqm
  rhdf5::h5createGroup(outname, paste0("/",
                                       site,
                                       "/dp01/qfqm/isoH2o/h2o",
                                       standard,
                                       "_03m"))

  std_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/",
                                      site,
                                      "/dp01/qfqm/isoH2o/h2o",
                                      standard,
                                      "_03m"))

  data_out_all <- do.call(rbind, list(dlta18OH2o[[2]],
                                      dlta2HH2o[[2]],
                                      pres[[2]],
                                      presEnvHut[[2]],
                                      rhEnvHut[[2]],
                                      rtioMoleWetH2o[[2]],
                                      rtioMoleWetH2oEnvHut[[2]],
                                      temp[[2]],
                                      tempEnvHut[[2]]))

  std <- base::split(data_out_all, factor(data_out_all$varname))

  # and write out as a dataframe.
  lapply(names(std), function(x) {
    rhdf5::h5writeDataset(obj = std[[x]],
                                     h5loc = std_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)
                                 })

  rhdf5::H5Gclose(std_outloc)

  # write ucrt
  rhdf5::h5createGroup(outname, paste0("/",
                                       site,
                                       "/dp01/ucrt/isoH2o/h2o",
                                       standard,
                                       "_03m"))

  std_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/",
                                      site,
                                      "/dp01/ucrt/isoH2o/h2o",
                                      standard,
                                      "_03m"))

  data_out_all <- do.call(rbind, list(dlta18OH2o[[3]],
                                      dlta2HH2o[[3]],
                                      pres[[3]],
                                      presEnvHut[[3]],
                                      rhEnvHut[[3]],
                                      rtioMoleWetH2o[[3]],
                                      rtioMoleWetH2oEnvHut[[3]],
                                      temp[[3]],
                                      tempEnvHut[[3]]))

  std <- base::split(data_out_all, factor(data_out_all$varname))

  # and write out as a dataframe.
  lapply(names(std), function(x) {
    rhdf5::h5writeDataset(obj = std[[x]],
                                     h5loc = std_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})

  rhdf5::H5Gclose(std_outloc)
}
