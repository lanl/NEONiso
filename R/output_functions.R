# output_functions.R
# functions that write data to hdf5 output files.

setup_output_file <- function(outname, site, analyte) {
  
  # check to make sure first letter of analyte is capitalized,
  # or capitalize if it's not (also make sure it's co2 or h2o)
  if (analyte == 'co2' | analyte == 'h2o') {
    analyte <- paste0(toupper(substring(analyte,1,1)),substring(analyte,2))
  } else if (analyte != 'Co2' & analyte != 'H2o') {
    stop("Invalid analyte selected in setup output file.")
  }
  
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
  
}



# write out calibrated reference data.



#low
rhdf5::h5createGroup(outname,
                     paste0("/", site, "/dp01/data/isoCo2/co2Low_09m"))

low_outloc <- rhdf5::H5Gopen(fid,
                             paste0("/", site, "/dp01/data/isoCo2/co2Low_09m"))

low <- rhdf5::h5read(inname,
                     paste0("/", site, "/dp01/data/isoCo2/co2Low_09m"))

#  low <- calibrate_standards_carbon(out, low, R_vpdb, f)
low <- calibrate_standards_carbon(out, low, correct_bad_refvals = TRUE,
                                  site = site, refGas = "low")

# loop through each variable amb.data.list and write out as a dataframe.
lapply(names(low), function(x) {
  rhdf5::h5writeDataset.data.frame(obj = low[[x]],
                                   h5loc = low_outloc,
                                   name = x,
                                   DataFrameAsCompound = TRUE)})

rhdf5::H5Gclose(low_outloc)






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

