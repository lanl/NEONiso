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
