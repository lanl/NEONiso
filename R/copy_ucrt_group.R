#' Title
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data.list
#' @param outname
#' @param site
#' @param file
#'
#' @return
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
