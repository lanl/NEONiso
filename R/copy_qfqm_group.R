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
