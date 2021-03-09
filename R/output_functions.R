# output_functions.R
# functions that write data to hdf5 output files.

#' Title
#'
#' @param analyte 
#'
#' @return
#' @export
#'
#' @examples
validate_analyte <- function(analyte) {
  # helper function to make sure all of the variaous output functions are consistent.
  # check to make sure first letter of analyte is capitalized,
  # or capitalize if it's not (also make sure it's co2 or h2o)
  if (analyte == 'co2' | analyte == 'h2o') {
    analyte <- paste0(toupper(substring(analyte,1,1)),substring(analyte,2))
  } else if (analyte != 'Co2' & analyte != 'H2o') {
    stop("Invalid analyte selected in setup output file.")
  }
  
  return(analyte)
}


#' Title
#'
#' @param outname 
#' @param site 
#' @param analyte 
#'
#' @return
#' @export
#'
#' @examples
setup_output_file <- function(outname, site, analyte) {
  
  analyte <- validate_analyte(analyte)
  
  rhdf5::h5createFile(outname)
  rhdf5::h5createGroup(outname, paste0("/", site))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/data/iso",analyte))
  
  rhdf5::h5closeAll()
  
  # copy attributes from source file and write to output file.
  fid <- rhdf5::H5Fopen(outname)
  tmp <- rhdf5::h5readAttributes(inname, paste0("/", site))
  
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

#' Title
#'
#' @param outname 
#' @param calDf 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
write_carbon_calibration_data <- function(outname, site, calDf, method) {

  print("Writing calibration parameters...")
  rhdf5::h5createGroup(outname,
                       paste0("/", site, "/dp01/data/isoCo2/calData"))

  fid <- rhdf5::H5Fopen(outname)
  co2_cal_outloc <- rhdf5::H5Gopen(fid,
                                   paste0("/", site, "/dp01/data/isoCo2/calData"))

  if (method == "Bowling_2003") {
    rhdf5::h5writeDataset.data.frame(obj = calDf,
                                     h5loc = co2_cal_outloc,
                                     name = "calGainsOffsets",
                                     DataFrameAsCompound = TRUE)
  } else if (method == "linreg") {
    rhdf5::h5writeDataset.data.frame(obj = calDf,
                                     h5loc = co2_cal_outloc,
                                     name = "calRegressions",
                                     DataFrameAsCompound = TRUE)
  }

  rhdf5::H5Gclose(co2_cal_outloc)

  # close the group and the file
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()

}


#' Title
#'
#' @param outname 
#' @param site 
#' @param amb_data_list 
#'
#' @return
#' @export
#'
#' @examples
write_carbon_ambient_data <- function(outname, site, amb_data_list) {
  
  print("Writing calibrated ambient data...")
  fid <- rhdf5::H5Fopen(outname)
  
  for (i in 1:length(amb_data_list)) {
    amb_data_subset <- amb_data_list[i]

    co2_data_outloc <- rhdf5::H5Gcreate(fid,
                                      paste0("/", site, "/dp01/data/isoCo2/", 
                                             names(amb_data_subset)))
    
    amb_data_subset <- amb_data_subset[[1]] # list hack
    
    # loop through variables in list amb_data_list and write out as a dataframe.
    lapply(names(amb_data_subset), function(x) {
      rhdf5::h5writeDataset.data.frame(obj = amb_data_subset[[x]],
                                       h5loc = co2_data_outloc,
                                       name = x,
                                       DataFrameAsCompound = TRUE)})
    
    rhdf5::H5Gclose(co2_data_outloc)
  }
  
  # close all open handles.
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()  
  
}


#' Title
#'
#' @param inname 
#' @param outname 
#' @param site 
#' @param analyte 
#'
#' @return
#' @export
#'
#' @examples
write_qfqm <- function(inname, outname, site, analyte) {
  
  analyte <- validate_analyte(analyte)
  
  print("Copying qfqm...")
  # copy over ucrt and qfqm groups as well.
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/qfqm/iso",analyte))
  qfqm <- rhdf5::h5read(inname, paste0("/", site, "/dp01/qfqm/iso",analyte))
  
  if (analyte == 'Co2') {
    lapply(names(qfqm), function(x) {
      copy_qfqm_group(data_list = qfqm[[x]],
                      outname = x,
                      file = outname,
                      site = site,
                      species = "CO2")})
  } else if (analyte == 'H2o') {
    lapply(names(qfqm), function(x) {
      copy_qfqm_group(data_list = qfqm[[x]],
                      outname = x,
                      file = outname,
                      site = site, 
                      species = "H2O")})
  }
  
  rhdf5::h5closeAll()

}

#' Title
#'
#' @param inname 
#' @param outname 
#' @param site 
#' @param analyte 
#'
#' @return
#' @export
#'
#' @examples
write_ucrt <- function(inname, outname, site, analyte) {
  
  analyte <- validate_analyte(analyte)
  
  print("Copying ucrt...")
  # copy over ucrt and qfqm groups as well.
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/"))
  rhdf5::h5createGroup(outname, paste0("/", site, "/dp01/ucrt/iso",analyte))
  ucrt <- rhdf5::h5read(inname, paste0("/", site, "/dp01/ucrt/iso",analyte))
  
  if (analyte == 'Co2') {
    lapply(names(ucrt), function(x) {
      copy_ucrt_group(data_list = ucrt[[x]],
                      outname = x,
                      file = outname,
                      site = site,
                      species = "CO2")})
  } else if (analyte == 'H2o') {
    lapply(names(ucrt), function(x) {
      copy_ucrt_group(data_list = ucrt[[x]],
                      outname = x,
                      file = outname,
                      site = site, 
                      species = "H2O")})
  }
  
  rhdf5::h5closeAll()
  
}


#' Title
#'
#' @param inname 
#' @param outname 
#' @param site 
#' @param calDf 
#'
#' @return
#' @export
#'
#' @examples
write_carbon_reference_data <- function(inname, outname, site, calDf) {
  
  print("Writing calibrated reference data...")
  calibrate_carbon_reference_data(inname, outname, "Low", site, calDf)
  calibrate_carbon_reference_data(inname, outname, "Med", site, calDf)
  calibrate_carbon_reference_data(inname, outname, "High", site, calDf)
  
}


#' Title
#'
#' @param inname 
#' @param outname 
#' @param standard 
#' @param site 
#' @param calDf 
#'
#' @return
#' @export
#'
#' @examples
calibrate_carbon_reference_data <- function(inname, outname,
                                     standard, site, calDf)  {
  
  std <- rhdf5::h5read(inname,
                       paste0("/", site, "/dp01/data/isoCo2/co2",standard,"_09m"))

  std <- calibrate_standards_carbon(calDf, std, correct_bad_refvals = TRUE,
                                    site = site, refGas = standard)

  
  fid <- rhdf5::H5Fopen(outname)
  rhdf5::H5Gcreate(fid, paste0("/", site, "/dp01/data/isoCo2/co2",standard,"_09m"))
  std_outloc <- rhdf5::H5Gopen(fid,
                               paste0("/", site, "/dp01/data/isoCo2/co2",standard,"_09m"))
  # loop through each variable amb.data.list and write out as a dataframe.
  lapply(names(std), function(x) {
    rhdf5::h5writeDataset.data.frame(obj = std[[x]],
                                     h5loc = std_outloc,
                                     name = x,
                                     DataFrameAsCompound = TRUE)})

  rhdf5::H5Gclose(std_outloc)
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()
}

#' copy_qfqm_group
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data_list List of groups to retrieve qfqm data from.
#' @param outname Output filename. Inhereted from a calibrate function.
#' @param site Four-letter NEON site code.
#' @param file Input filename. Inhereted from one of the calibrate functions.
#' @param species CO2 or H2O? Same function used for both CO2 and H2O isotopes.
#'
#' @return Nothing to the workspace, but copies qfqm group from input file to
#'         output file.
#' @export
#'
copy_qfqm_group <- function(data_list, outname, site, file, species) {

  # create hdf5 structure for these variables.
  fid <- rhdf5::H5Fopen(file)

  if (species == "CO2") {
    co2_data_outloc <- rhdf5::H5Gcreate(fid,
                                        paste0("/", site, "/dp01/qfqm/isoCo2/", outname))

    # loop through each of the variables in list amb.data.list
    # and write out as a dataframe.
    lapply(names(data_list), function(x) {
      rhdf5::h5writeDataset.data.frame(obj = data_list[[x]],
                                       h5loc = co2_data_outloc,
                                       name = x,
                                       DataFrameAsCompound = TRUE)})

  } else if (species == "H2O") {

    h2o_data_outloc <- rhdf5::H5Gcreate(fid,
                                        paste0("/", site, "/dp01/qfqm/isoH2o/", outname))

    # loop through each of the variables in list amb.data.list
    # and write out as a dataframe.
    lapply(names(data_list), function(x) {
      rhdf5::h5writeDataset.data.frame(obj = data_list[[x]],
                                       h5loc = h2o_data_outloc,
                                       name = x,
                                       DataFrameAsCompound = TRUE)})

  }

  # close all open handles.
  rhdf5::h5closeAll()

}

#' copy_ucrt_group
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param file Input file name.
#' @param data_list List of groups to retrieve ucrt data from.
#' @param species H2O or CO2.
#'
#' @return Nothing to the workspace, but copies ucrt group from input file to
#'         output file.
#' @export
#'
copy_ucrt_group <- function(data_list, outname, site, file, species) {

  # create hdf5 structure for these variables.
  fid <- rhdf5::H5Fopen(file)

  if (species == "CO2") {

    co2_data_outloc <- rhdf5::H5Gcreate(fid,
                                        paste0("/", site, "/dp01/ucrt/isoCo2/", outname))

    # loop through each variable in amb.data.list and write out as a dataframe
    lapply(names(data_list), function(x) {
      rhdf5::h5writeDataset.data.frame(obj = data_list[[x]],
                                       h5loc = co2_data_outloc,
                                       name = x,
                                       DataFrameAsCompound = TRUE)})

  } else if (species == "H2O") {

    h2o_data_outloc <- rhdf5::H5Gcreate(fid,
                                        paste0("/", site, "/dp01/ucrt/isoH2o/", outname))

    # loop through each variable in amb.data.list and write out as a dataframe.
    lapply(names(data_list), function(x) {
      rhdf5::h5writeDataset.data.frame(obj = data_list[[x]],
                                       h5loc = h2o_data_outloc,
                                       name = x,
                                       DataFrameAsCompound = TRUE)})

  }

  # close all open handles.
  rhdf5::h5closeAll()

}
