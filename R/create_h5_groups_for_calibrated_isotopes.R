#----------------------------------------------------------------------------------
#' create_h5_groups_for_calibrated_isotopes
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param file 
#' @param site 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
create_h5_groups_for_calibrated_isotopes <- function(file,site) {
  # folder structure generally:
  # sitename/dp01iso/data/{isoH2o,isoCo2}/{each tower level, cal data}
  # this function creates the dp01iso, data, and iso* levels. the tower
  # levels and cal data are datasets and can be created in other fns.
  
  # print status
  print("Creating hdf5 group structure for calibrated isotope data...")
  
  # H5Gcreate won't operate recursively, so we need to do each level separately.
  # create file id
  fid <- H5Fopen(file) # note: required to be the actual file!
  
  # create dp01iso level.
  g1 <- H5Gcreate(fid,paste0('/',site,'/dp01iso/'))
  
  # create the data level
  g2 <- H5Gcreate(g1,'data')
  
  # create the iso levels.
  H5Gcreate(g2,'isoCo2')
  H5Gcreate(g2,'isoH2o')
  
  # close the file.
  H5Fclose(fid)
}