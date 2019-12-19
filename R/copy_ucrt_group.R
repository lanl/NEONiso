#' Title
#'
#' @param data.list 
#' @param outname 
#' @param site 
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
copy_ucrt_group <- function(data.list,outname,site,file) {
  
  # create hdf5 structure for these variables.
  fid <- H5Fopen(file)

  co2.data.outloc <- H5Gcreate(fid,paste0('/',site,'/dp01/ucrt/isoCo2/',outname))
  
  # loop through each of the variables in list amb.data.list and write out as a dataframe.
  lapply(names(data.list),function(x) {
    h5writeDataset.data.frame(obj=data.list[[x]],
                              h5loc=co2.data.outloc,
                              name=x,
                              DataFrameAsCompound = TRUE)})
  
  # close all open handles.
  h5closeAll()
}