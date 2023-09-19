# restructure_data
#' ingest_data
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname A file (or list of files) to extract data from for calibration.
#' @param analyte Carbon (Co2) or water (H2o)?
#' @param name_fix Fix to data frame required for next-generation calibration
#'                 functions, but breaks old 'by_month()' functions. This
#'                 parameter provides a necessary work around until these
#'                 functions are removed.
#' @param avg The averaging interval to extract, in minutes.
#'
#' @return List of data frames, taken from files specified in `inname`
#' @export
#'
#' @importFrom stats setNames
#' @importFrom utils packageVersion
#' @importFrom magrittr %>%
ingest_data <- function(inname, analyte, name_fix = TRUE, avg) {

  # this function needs to:
  # 1. read in and stack variables.
  # 2. restructure them to have the same setup as output files.
  # 3. return list structure where elements are: a) ambient data,
  # b) ambient qfqm, c) ambient ucrt, d-f) same, but for ref vars.

  analyte <- validate_analyte(analyte)

  # read attributes from (first file in) inname
  site <- rhdf5::h5ls(inname[1], recursive = 1)[1, 2]
  attrs <- rhdf5::h5readAttributes(inname[1], name = paste0("/", site))

  nheights <- attrs$LvlMeasTow

  if (analyte == "Co2") {

    if (packageVersion("neonUtilities") >= "2.3.0") {
      data <- neonUtilities::stackEddy(inname,
                                       avg = avg,
                                       level = "dp01",
                                       var = "isoCo2",
                                       useFasttime = TRUE)[[1]]
    } else if (packageVersion("neonUtilities") >= "2.1.1" &&
               packageVersion("neonUtilities") < "2.3.0") {
      data <- neonUtilities::stackEddy(inname,
                                       avg = avg,
                                       level = "dp01",
                                       var = "isoCo2")[[1]]
    } else {
      data <- neonUtilities::stackEddy(inname,
                                       avg = avg,
                                       level = "dp01")[[1]]
    }

    # filter data and remove rows that are all NaNs:
    data <- data %>%
      dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                    tidyselect::contains("isoCo2"))

    # stack required variables.
    ambToStack <- c("dlta13CCo2", "pres", "presEnvHut", "rhEnvHut",
                    "rtioMoleDry12CCo2", "rtioMoleDry13CCo2", "rtioMoleDryCo2",
                    "rtioMoleDryH2o", "rtioMoleWet12CCo2", "rtioMoleWet13CCo2",
                    "rtioMoleWetCo2", "rtioMoleWetH2o", "rtioMoleWetH2oEnvHut",
                    "temp", "tempEnvHut")

    refToStack <- base::sort(base::append(ambToStack,
                                          c("dlta13CCo2Refe",
                                            "rtioMoleDryCo2Refe")))

    # split data into ambient and reference data frames.
    ambient <- data %>%
      dplyr::filter(.data$verticalPosition %in% c("010", "020", "030", "040",
                                                  "050", "060", "070", "080"))

    # check how many heights are present in ambient.
    if (length(unique(ambient$verticalPosition)) < nheights) {
      print("Height missing, attempting to resolve:")

      # determine which height is missing:
      hgts_present <- seq(from = 1, to = nheights, by = 1) %in%
                      (as.numeric(unique(ambient$verticalPosition)) / 10)

      hgts_absentl <- !hgts_present

      hgts_absent <- seq(from = 1, to = nheights, by = 1)[hgts_absentl]

      # add a row to data, and then change verticalPosition to missing heights
      for (i in hgts_absent) {
        target_row <- nrow(ambient) + 1
        ambient[target_row, ] <- NA
        ambient[target_row, "verticalPosition"] <- paste0("0", i, "0")
      }
    }

    reference <- data %>%
      dplyr::filter(.data$verticalPosition %in%
                    c("co2Low", "co2Med", "co2High"))

  } else if (analyte == "H2o") {
    stop("ingest_data does not work yet for H2o.")
  }

  ambi_by_height <- base::split(ambient, factor(ambient$verticalPosition))
  refe_by_height <- base::split(reference, factor(reference$verticalPosition))

  #-------------------------
  # RESTRUCTURE AMBIENT
  # feed into restructure carbon variables:
  ambi_out <- lapply(ambi_by_height,
                function(y) {
                  lapply(ambToStack,
                        function(x) {
                          restructure_carbon_variables(y,
                                                       varname = x,
                                                       mode = "ambient",
                                                       group = "data")
                                    })
                            })

  # loop through again to rename data frames.
  ambi_out <- lapply(ambi_out, setNames, ambToStack)

  # check length, and error out if a height has been dropped.
  test_var <- identical(as.integer(nheights), length(ambi_out))

  if (!test_var) {
    stop("Tower height dropped somewhere within ingest_data...")
  }

  #-------------------------
  # RESTRUCTURE REFERENCE
  # feed into restructure carbon variables:
  refe_out <- lapply(refe_by_height,
                    function(y) {
                      lapply(refToStack,
                            function(x) {
                              restructure_carbon_variables(y,
                                                           varname = x,
                                                           mode = "reference",
                                                           group = "data")
                                        })
                                }) # replace the of the variables.

  # loop through again to rename data frames.
  refe_out <- lapply(refe_out, setNames, refToStack)

  # remove variable name from ambi_out data frames -
  # could be used here though to validate in future version.
  # variable name has been removed in restructure_carbon_variables
  # - could move it back here to validate!

  #changing average period in numeric to characters, e.g. 9 to 09m
  avgChar <- paste0("0", avg, "m")

  # get number of heights
  if (nrow(ambient) > 0) {
    heights <- unique(ambient$verticalPosition) # not that efficient, but needed
    names_vector <- vector()
    for (i in 1:length(heights)) {
      names_vector[i] <- paste0("000_0", i, "0_", avgChar)
    }
    names(ambi_out) <- names_vector
  }

  if (name_fix) {
    # append _09m to refe_out....MAY CAUSE PROBLEMS FOR OTHER METHODS!!!!!!
    names(refe_out) <- paste0(names(refe_out), "_", avgChar)
  }

  output <- list(ambi_out, refe_out, reference)
  names(output) <- c("ambient", "reference", "refe_stacked")

  return(output)
}

#-----------------------------------------
#' restructure_carbon_variables
#'
#' @param varname Which variable are we applying this function to? There's
#'                a list of ~10 common ones to write to the hdf5 file.
#' @param dataframe Input data.frame, from `neonUtilities::stackEddy`
#' @param mode Are we fixing a reference data frame or an ambient data frame?
#' @param group Data, ucrt, or qfqm?
#'
#' @return data.frame formatted for output to hdf5 file.
#' @export
#'
#' @importFrom magrittr %>%

restructure_carbon_variables <- function(dataframe,
                                        varname,
                                        mode,
                                        group) {

  if (mode != "reference" & mode != "ambient") {

    stop("Invalid selection to mode argument.")

  } else if (mode == "reference") {

    if (group == "data") {

      output <- dataframe %>%
        dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                      tidyselect::starts_with(paste0("data.isoCo2.",
                                                     varname,
                                                     "."))) %>%
        dplyr::filter(!(.data$verticalPosition %in% c("010", "020", "030",
                                                      "040", "050", "060",
                                                      "070", "080"))) %>%
        dplyr::rename(mean = paste0("data.isoCo2.", varname, ".mean"),
                      min  = paste0("data.isoCo2.", varname, ".min"),
                      max  = paste0("data.isoCo2.", varname, ".max"),
                      vari = paste0("data.isoCo2.", varname, ".vari"),
                      numSamp = paste0("data.isoCo2.", varname, ".numSamp")) %>%
        dplyr::mutate(dom = lubridate::day(.data$timeBgn),
                      yr  = lubridate::year(.data$timeBgn),
                      mn  = lubridate::month(.data$timeBgn)) %>%
        dplyr::group_by(.data$yr, .data$mn, .data$dom) %>%
        dplyr::filter(.data$numSamp > 30 | is.na(.data$numSamp)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"dom", -"yr", -"mn", -"verticalPosition")

    } else if (group == "qfqm") {

      if (!grepl("Refe", varname)) {

        output <- dataframe %>%
          dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                        tidyselect::starts_with(paste0("qfqm.isoCo2.",
                                                       varname,
                                                       "."))) %>%
          dplyr::filter(!(.data$verticalPosition %in% c("010", "020", "030",
                                                        "040", "050", "060",
                                                        "070", "080"))) %>%
          dplyr::rename(qfFinl = paste0("qfqm.isoCo2.", varname, ".qfFinl")) %>%
          dplyr::mutate(varname = varname)

      }

    } else if (group == "ucrt") {

      if (!grepl("Refe", varname)) {

        output <- dataframe %>%
          dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                        tidyselect::starts_with(paste0("ucrt.isoCo2.",
                                                       varname,
                                                       "."))) %>%
          dplyr::filter(!(.data$verticalPosition %in% c("010", "020", "030",
                                                        "040", "050", "060",
                                                        "070", "080"))) %>%
          dplyr::rename(mean = paste0("ucrt.isoCo2.", varname, ".mean"),
                        vari = paste0("ucrt.isoCo2.", varname, ".vari"),
                        se   = paste0("ucrt.isoCo2.", varname, ".se")) %>%
          dplyr::mutate(varname = varname)

      }
    }

  } else if (mode == "ambient") {
    output <- dataframe %>%
      dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                    tidyselect::starts_with(paste0("data.isoCo2.",
                                                   varname,
                                                   "."))) %>%
      dplyr::filter(!(.data$verticalPosition %in% c("co2Low", "co2Med",
                                                    "co2High", "co2Arch"))) %>%
      dplyr::rename(mean = paste0("data.isoCo2.", varname, ".mean"),
                    min  = paste0("data.isoCo2.", varname, ".min"),
                    max  = paste0("data.isoCo2.", varname, ".max"),
                    vari = paste0("data.isoCo2.", varname, ".vari"),
                    numSamp = paste0("data.isoCo2.", varname, ".numSamp")) %>%
      dplyr::select(-"verticalPosition")

  }

  # stackEddy will have converted time to posixct - covert back here.
  output$timeBgn <- convert_POSIXct_to_NEONhdf5_time(output$timeBgn)
  output$timeEnd <- convert_POSIXct_to_NEONhdf5_time(output$timeEnd)

  return(output)
}

#-----------------------------------------
#' restructure_water_variables
#'
#' @param varname Which variable are we applying this function to? There's
#'                a list of ~10 common ones to write to the hdf5 file.
#' @param dataframe Input data.frame, from `neonUtilities::stackEddy`
#' @param mode Are we fixing a reference data frame or an ambient data frame?
#'
#' @return data.frame formatted for output to hdf5 file.
#' @export
#'
restructure_water_variables <- function(dataframe,
                                        varname,
                                        mode) {

  # ensure that varname is a string but standard is a data.frame
  if (!is.character(varname)) {
    stop("varname must be a string")
  } else if ((!is.data.frame(dataframe) & mode == "reference") |
             (!is.list(dataframe) & mode == "ambient")) {
    stop("dataframe argument must be a data.frame (reference mode) or list (ambient mode)")
  }

  if (mode != "reference" & mode != "ambient") {
    stop("Invalid selection to mode argument.")
  } else if (mode == "reference") {
    output1 <- dataframe %>%
      dplyr::select("timeBgn", "timeEnd",
                    tidyselect::starts_with(paste0("data.isoH2o.",
                                                   varname,
                                                   "."))) %>%
      dplyr::rename(mean = paste0("data.isoH2o.", varname, ".mean"),
                    min  = paste0("data.isoH2o.", varname, ".min"),
                    max  = paste0("data.isoH2o.", varname, ".max"),
                    vari = paste0("data.isoH2o.", varname, ".vari"),
                    numSamp = paste0("data.isoH2o.", varname, ".numSamp")) %>%
      dplyr::mutate(varname = varname) %>%
      dplyr::mutate(dom = lubridate::day(.data$timeBgn),
                    yr  = lubridate::year(.data$timeBgn),
                    mn  = lubridate::month(.data$timeBgn)) %>%
      dplyr::group_by(.data$yr, .data$mn, .data$dom) %>%
      dplyr::filter(.data$numSamp > 30 | is.na(.data$numSamp)) %>%
      dplyr::slice(tail(dplyr::row_number(), 3)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"dom", -"yr", -"mn")

    if (!grepl("Refe", varname)) {
      output2 <- dataframe %>%
        dplyr::select("timeBgn", "timeEnd",
                      tidyselect::starts_with(paste0("qfqm.isoH2o.",
                                                     varname, "."))) %>%
        dplyr::rename(qfFinl = paste0("qfqm.isoH2o.", varname, ".qfFinl")) %>%
        dplyr::mutate(varname = varname) %>%
        dplyr::filter(.data$timeBgn %in% output1$timeBgn)

      output3 <- dataframe %>%
        dplyr::select("timeBgn", "timeEnd",
                      tidyselect::starts_with(paste0("ucrt.isoH2o.",
                                                     varname,
                                                     "."))) %>%
        dplyr::rename(mean = paste0("ucrt.isoH2o.", varname, ".mean"),
                      vari = paste0("ucrt.isoH2o.", varname, ".vari"),
                      se   = paste0("ucrt.isoH2o.", varname, ".se")) %>%
        dplyr::mutate(varname = varname) %>%
        dplyr::filter(.data$timeBgn %in% output1$timeBgn)

    } else {
      output2 <- output3 <- NULL
    }

  } else if (mode == "ambient") {
    output1 <- dataframe[[1]] %>%
      dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                    tidyselect::starts_with(paste0("data.isoH2o.",
                                                   varname, "."))) %>%
      dplyr::filter(!(.data$verticalPosition %in% c("co2Low", "co2Med",
                                                    "co2High", "co2Arch"))) %>%
      dplyr::rename(mean = paste0("data.isoH2o.", varname, ".mean"),
                    min  = paste0("data.isoH2o.", varname, ".min"),
                    max  = paste0("data.isoH2o.", varname, ".max"),
                    vari = paste0("data.isoH2o.", varname, ".vari"),
                    numSamp = paste0("data.isoH2o.", varname, ".numSamp")) %>%
      dplyr::mutate(varname = varname)

    output2 <- output3 <- NULL

  }

  # stackEddy will have converted time to posixct - covert back here.
  output1$timeBgn <- convert_POSIXct_to_NEONhdf5_time(output1$timeBgn)
  output1$timeEnd <- convert_POSIXct_to_NEONhdf5_time(output1$timeEnd)

  if (!grepl("Refe", varname)) {
    output2$timeBgn <- convert_POSIXct_to_NEONhdf5_time(output2$timeBgn)
    output2$timeEnd <- convert_POSIXct_to_NEONhdf5_time(output2$timeEnd)

    output3$timeBgn <- convert_POSIXct_to_NEONhdf5_time(output3$timeBgn)
    output3$timeEnd <- convert_POSIXct_to_NEONhdf5_time(output3$timeEnd)
  }

  return(list(output1, output2, output3))
}


#' restructure_ambient_data
#'
#' @param inpath Folder containing data to stack.
#' @param analyte Carbon (Co2) or water (H2o)?
#'
#' @return List of data extracted from files listed in inpath.
#'
restructure_ambient_data <- function(inpath, analyte) {
  # stack data available for a given site into a single timeseries.
  # a target for improvement: don't list each required variable separately,
  # but generate dynamically.

  analyte <- validate_analyte(analyte)

  if (analyte == "Co2") {

    # split first by height
    #data <- do.call(rbind,data)
    #data_by_height <- base::split(data, factor(data$verticalPosition))

  } else if (analyte == "H2o") {

    dlta18O_list <- neonUtilities::stackEddy(inpath,
                                             level = "dp01",
                                             var = "dlta18OH2o",
                                             avg = 9)
    dlta18OH2o <- restructure_water_variables(dlta18O_list,
                                              "dlta18OH2o",
                                              "ambient")
    dlta18OH2o[[1]] <- dlta18OH2o[[1]][rowSums(is.na(dlta18OH2o[[1]])) < 5, ]

    dlta2H_list <- neonUtilities::stackEddy(inpath,
                                            level = "dp01",
                                            var = "dlta2HH2o",
                                            avg = 9)
    dlta2HH2o <- restructure_water_variables(dlta2H_list,
                                             "dlta2HH2o",
                                             "ambient")
    dlta2HH2o[[1]] <- dlta2HH2o[[1]][rowSums(is.na(dlta2HH2o[[1]])) < 5, ]

    pres_list <- neonUtilities::stackEddy(inpath,
                                          level = "dp01",
                                          var = "pres",
                                          avg = 9)
    pres <- restructure_water_variables(pres_list, "pres", "ambient")
    pres[[1]] <- pres[[1]][rowSums(is.na(pres[[1]])) < 5, ]

    presEnvHut_list <- neonUtilities::stackEddy(inpath,
                                                level = "dp01",
                                                var = "presEnvHut",
                                                avg = 9)
    presEnvHut <- restructure_water_variables(presEnvHut_list,
                                              "presEnvHut",
                                              "ambient")
    presEnvHut[[1]] <- presEnvHut[[1]][rowSums(is.na(presEnvHut[[1]])) < 5, ]

    rhEnvHut_list <- neonUtilities::stackEddy(inpath,
                                              level = "dp01",
                                              var = "rhEnvHut",
                                              avg = 9)
    rhEnvHut <- restructure_water_variables(rhEnvHut_list,
                                            "rhEnvHut",
                                            "ambient")
    rhEnvHut[[1]] <- rhEnvHut[[1]][rowSums(is.na(rhEnvHut[[1]])) < 5, ]

    rtioMoleDryH2o_list <- neonUtilities::stackEddy(inpath,
                                                    level = "dp01",
                                                    var = "rtioMoleDryH2o",
                                                    avg = 9)
    rtioMoleDryH2o <- restructure_water_variables(rtioMoleDryH2o_list,
                                                  "rtioMoleDryH2o",
                                                  "ambient")
    rtioMoleDryH2o[[1]] <- rtioMoleDryH2o[[1]][rowSums(is.na(rtioMoleDryH2o[[1]])) < 5, ]

    rtioMoleWetH2o_list <- neonUtilities::stackEddy(inpath,
                                                    level = "dp01",
                                                    var = "rtioMoleWetH2o",
                                                    avg = 9)
    rtioMoleWetH2o <- restructure_water_variables(rtioMoleWetH2o_list,
                                                  "rtioMoleWetH2o",
                                                  "ambient")
    rtioMoleWetH2o[[1]] <- rtioMoleWetH2o[[1]][rowSums(is.na(rtioMoleWetH2o[[1]])) < 5, ]

    rtioMoleWetH2oEnvHut_list <- neonUtilities::stackEddy(inpath,
                                                          level = "dp01",
                                                          var = "rtioMoleWetH2oEnvHut",
                                                          avg = 9)
    rtioMoleWetH2oEnvHut <- restructure_water_variables(rtioMoleWetH2oEnvHut_list,
                                                        "rtioMoleWetH2oEnvHut",
                                                        "ambient")
    rtioMoleWetH2oEnvHut[[1]] <- rtioMoleWetH2oEnvHut[[1]][rowSums(is.na(rtioMoleWetH2oEnvHut[[1]])) < 5, ]

    temp_list <- neonUtilities::stackEddy(inpath,
                                          level = "dp01",
                                          var = "temp",
                                          avg = 9)
    temp <- restructure_water_variables(temp_list,
                                        "temp",
                                        "ambient")
    temp[[1]] <- temp[[1]][rowSums(is.na(temp[[1]])) < 5, ]

    tempEnvHut_list <- neonUtilities::stackEddy(inpath,
                                                level = "dp01",
                                                var = "tempEnvHut",
                                                avg = 9)
    tempEnvHut <- restructure_water_variables(tempEnvHut_list,
                                              "tempEnvHut",
                                              "ambient")
    tempEnvHut[[1]] <- tempEnvHut[[1]][rowSums(is.na(tempEnvHut[[1]])) < 5, ]

    data_out_all <- do.call(rbind, list(dlta18OH2o[[1]],
                                        dlta2HH2o[[1]],
                                        pres[[1]],
                                        presEnvHut[[1]],
                                        rhEnvHut[[1]],
                                        rtioMoleDryH2o[[1]],
                                        rtioMoleWetH2o[[1]],
                                        rtioMoleWetH2oEnvHut[[1]],
                                        temp[[1]],
                                        tempEnvHut[[1]]))

    # split first by height
    data_by_height <- base::split(data_out_all,
                                  factor(data_out_all$verticalPosition))
  }

  # get number of heights
  heights <- unique(data_out_all$verticalPosition)
  names_vector <- vector()
  for (i in 1:length(heights)) {
    names_vector[i] <- paste0("000_0", i, "0_09m")
  }

  names(data_by_height) <- names_vector

  # remove verticalPosition column
  data_by_height <- lapply(data_by_height,
                           function(x) {
                            dplyr::select(x, -"verticalPosition")
                            })

  data_by_height_by_var <- lapply(data_by_height,
                                  function(x) {
                                    base::split(x, factor(x$varname))
                                    })

  # return list of data by height by var
  return(data_by_height_by_var)
}


#' restructure_ambient_data2
#'
#' @param inpath Folder containing data to stack.
#' @param analyte Carbon (Co2) or water (H2o)?
#'
#' @return List of data extracted from files listed in inpath.
#'
restructure_ambient_data2 <- function(inpath, analyte) {
  # stack data available for a given site into a single timeseries.
  # a target for improvement: don't list each required variable separately,
  # but generate dynamically.

  analyte <- validate_analyte(analyte)

  if (analyte == "Co2") {

    # split first by height
    ## data_by_height <- base::split(data, factor(data$verticalPosition))

  } else if (analyte == "H2o") {

    dlta18O_list <- neonUtilities::stackEddy(inpath,
                                             level = "dp01",
                                             var = "dlta18OH2o",
                                             avg = 9)
    dlta18OH2o <- restructure_water_variables(dlta18O_list,
                                              "dlta18OH2o",
                                              "ambient")

    dlta2H_list <- neonUtilities::stackEddy(inpath,
                                            level = "dp01",
                                            var = "dlta2HH2o",
                                            avg = 9)
    dlta2HH2o <- restructure_water_variables(dlta2H_list,
                                             "dlta2HH2o",
                                             "ambient")

    pres_list <- neonUtilities::stackEddy(inpath,
                                          level = "dp01",
                                          var = "pres",
                                          avg = 9)
    pres <- restructure_water_variables(pres_list,
                                        "pres",
                                        "ambient")

    presEnvHut_list <- neonUtilities::stackEddy(inpath,
                                                level = "dp01",
                                                var = "presEnvHut",
                                                avg = 9)
    presEnvHut <- restructure_water_variables(presEnvHut_list,
                                              "presEnvHut",
                                              "ambient")

    rhEnvHut_list <- neonUtilities::stackEddy(inpath,
                                              level = "dp01",
                                              var = "rhEnvHut",
                                              avg = 9)
    rhEnvHut <- restructure_water_variables(rhEnvHut_list,
                                            "rhEnvHut",
                                            "ambient")

    rtioMoleWetH2o_list <- neonUtilities::stackEddy(inpath,
                                                    level = "dp01",
                                                    var = "rtioMoleWetH2o",
                                                    avg = 9)
    rtioMoleWetH2o <- restructure_water_variables(rtioMoleWetH2o_list,
                                                  "rtioMoleWetH2o",
                                                  "ambient")

    rtioMoleWetH2oEnvHut_list <- neonUtilities::stackEddy(inpath,
                                                          level = "dp01",
                                                          var = "rtioMoleWetH2oEnvHut",
                                                          avg = 9)
    rtioMoleWetH2oEnvHut <- restructure_water_variables(rtioMoleWetH2oEnvHut_list,
                                                        "rtioMoleWetH2oEnvHut",
                                                        "ambient")

    temp_list <- neonUtilities::stackEddy(inpath,
                                          level = "dp01",
                                          var = "temp",
                                          avg = 9)
    temp <- restructure_water_variables(temp_list,
                                        "temp",
                                        "ambient")

    tempEnvHut_list <- neonUtilities::stackEddy(inpath,
                                                level = "dp01",
                                                var = "tempEnvHut",
                                                avg = 9)
    tempEnvHut <- restructure_water_variables(tempEnvHut_list,
                                              "tempEnvHut",
                                              "ambient")

    data_out_all <- do.call(rbind, list(dlta18OH2o[[1]],
                                        dlta2HH2o[[1]],
                                        pres[[1]],
                                        presEnvHut[[1]],
                                        rhEnvHut[[1]],
                                        rtioMoleWetH2o[[1]],
                                        rtioMoleWetH2oEnvHut[[1]],
                                        temp[[1]],
                                        tempEnvHut[[1]]))

    # split first by height
    data_by_height <- base::split(data_out_all,
                                  factor(data_out_all$verticalPosition))

  }

  # get number of heights
  heights <- unique(data_out_all$verticalPosition)
  names_vector <- vector()
  for (i in 1:length(heights)) {
    names_vector[i] <- paste0("000_0", i, "0_09m")
  }

  names(data_by_height) <- names_vector

  # remove verticalPosition column
  data_by_height <- lapply(data_by_height,
                           function(x) {
                              dplyr::select(x, -"verticalPosition")
                              })
  data_by_height_by_var <- lapply(data_by_height,
                                  function(x) {
                                    base::split(x, factor(x$varname))
                                    })

  # return list of data by height by var
  return(data_by_height_by_var)
}
