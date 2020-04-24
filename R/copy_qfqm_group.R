#' copy_qfqm_group
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data.list 
#' @param outname Output filename. Inhereted from one of the calibrate functions.
#' @param site Four-letter NEON site code.
#' @param file Input filename. Inhereted from one of the calibrate functions.
#' @param species CO2 or H2O? Same function used for both CO2 and H2O isotopes.
#'
#' @return Nothing to the workspace, but copies qfqm group from input file to 
#'         output file.
#' @export
#'
copy_qfqm_group <- function(data.list, outname, site, file, species) {
  
  # create hdf5 structure for these variables.
  fid <- H5Fopen(file)

  if (species == "CO2") {
    co2.data.outloc <- H5Gcreate(fid, paste0("/", site, "/dp01/qfqm/isoCo2/", outname))
    
    # loop through each of the variables in list amb.data.list 
    # and write out as a dataframe.
    lapply(names(data.list), function(x) {
      h5writeDataset.data.frame(obj = data.list[[x]],
                                h5loc = co2.data.outloc,
                                name = x,
                                DataFrameAsCompound = TRUE)})
    
  } else if (species == "H2O") {

    h2o.data.outloc <- H5Gcreate(fid, paste0("/", site, "/dp01/qfqm/isoH2o/", outname))
    
    # loop through each of the variables in list amb.data.list 
    # and write out as a dataframe.
    lapply(names(data.list), function(x) {
      h5writeDataset.data.frame(obj = data.list[[x]],
                                h5loc = h2o.data.outloc,
                                name = x,
                                DataFrameAsCompound = TRUE)})
  }

  # close all open handles.
  h5closeAll()
}
