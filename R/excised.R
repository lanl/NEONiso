# nocov start

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

  lapply(names(qfqm),
         function(x) {
           copy_qfqm_group(data_list = qfqm[[x]],
                           outname = x,
                           file = outname,
                           site = site,
                           species = analyte)
         })

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

  lapply(names(ucrt),
         function(x) {
           copy_ucrt_group(data_list = ucrt[[x]],
                           outname = x,
                           file = outname,
                           site = site,
                           species = analyte)
         })

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
                                        paste0("/",
                                               site,
                                               "/dp01/qfqm/isoCo2/",
                                               outname))

    # loop through each of the variables in list amb.data.list
    # and write out as a dataframe.
    lapply(names(data_list),
           function(x) {
             rhdf5::h5writeDataset(obj = data_list[[x]],
                                   h5loc = co2_data_outloc,
                                   name = x,
                                   DataFrameAsCompound = TRUE)
           })

  } else if (species == "H2o") {

    h2o_data_outloc <- rhdf5::H5Gcreate(fid,
                                        paste0("/",
                                               site,
                                               "/dp01/qfqm/isoH2o/",
                                               outname))

    # loop through each of the variables in list amb.data.list
    # and write out as a dataframe.
    lapply(names(data_list),
           function(x) {
             rhdf5::h5writeDataset(obj = data_list[[x]],
                                   h5loc = h2o_data_outloc,
                                   name = x,
                                   DataFrameAsCompound = TRUE)
           })

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
    lapply(names(data_list),
           function(x) {
             rhdf5::h5writeDataset(obj = data_list[[x]],
                                   h5loc = co2_data_outloc,
                                   name = x,
                                   DataFrameAsCompound = TRUE)
           })

  } else if (species == "H2o") {

    h2o_data_outloc <- rhdf5::H5Gcreate(fid,
                                        paste0("/",
                                               site,
                                               "/dp01/ucrt/isoH2o/",
                                               outname))

    # loop through each variable in amb.data.list and write out as a dataframe.
    lapply(names(data_list),
           function(x) {
             rhdf5::h5writeDataset(obj = data_list[[x]],
                                   h5loc = h2o_data_outloc,
                                   name = x,
                                   DataFrameAsCompound = TRUE)
           })


  }

  # close all open handles.
  rhdf5::h5closeAll()

}

# nocov end
