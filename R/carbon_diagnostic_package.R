#' carbon_diagnostic_package.R
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data_path Provide path to where calibrated data from one site live.
#' @param which_sites Which NEON sites to run plots for? Default = all
#' @param plot_path Path to where output pdf plots should be written.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off pdf
#' @importFrom utils menu
#'
carbon_diagnostic_package <- function(data_path,
                                      plot_path,
                                      which_sites = "all") {

  # what plots would be good here:
  # 1) timeseries of calibration data
  # 2) timeseries of calibration regressions
  # 3) timeseries of ambient data
  # 4) monthly versions of 1-3.
  # all of these should be queried when running this function.

  # query re: calibration plots
  print("This function makes diagnostic plots of NEON carbon isotope data.")

  #-------------------------------------------------------
  # query for which plots.
  which_plots <- menu(c("Raw Calibration data - Monthly",
                        "Calibration Parameters - Monthly",
                        "Atmospheric Measurements - Monthly",
                        "Raw Calibration data - Full Timeseries",
                        "Calibration Parametrs - Full Timeseries",
                        "Atmospheric Measurements - Full Timeseries",
                        "All Monthly Plots",
                        "All Full Timeseries Plots",
                        "All Plots",
                        "Reference material distributions",
                        "CO2 Measurement Variance - Full Timeseries",
                        "d13C Measurement Variance - Full Timeseries",
                        "I've made a huge mistake."),
                      title = "Which plots should be run?")

  # allow graceful exit if want to stop.
  if (which_plots == 13) {

    stop()

  }

  #--------------------------------------------------------------
  # If we're still running, make a folder to hold diagnostic plots.
  out_folder <- paste0(plot_path, "/", Sys.time())

  dir.create(out_folder)

  #--------------------------------------------------------------
  # Find files common to each plotting script below.

  # find the files associated w/ that site.
  if (which_sites == "all") {

    slist <- list.files(data_path, pattern = ".h5", recursive = TRUE)

  } else {

    # check to see if *is* a neon site.
    neon_sites <- c(terrestrial_core_sites(),
                    terrestrial_relocatable_sites())

    if (!(which_sites %in% neon_sites)) {

      stop("Invalid NEON site selected!")

    } else {

      slist <- list.files(paste0(data_path, "/", which_sites), pattern = ".h5")

    } #neon_sites

  } # which_sites "all"

  # extract lists of domains, site codes, and year-month combos from file names
  slist.tmp <- strsplit(slist, split = ".", fixed = TRUE)

  domain <- sapply(slist.tmp, "[[", 2)
  sitecd <- sapply(slist.tmp, "[[", 3)
  yrmn   <- sapply(slist.tmp, "[[", 8)

  # validate sitecd. if single site was given, should have 1 unique value.
  if (which_sites != "all") {

    # check to see if length(unique(sitecd)) == 1
    if (length(unique(sitecd)) != 1) {

      stop("Single site selected by user, but multiple sites chosen here")

    }

  }

  #--------------------------------------------------------------
  # Set up logical tests to determine which plots below to run.
  # get data.

  # get vector of sites:
  unq_sites <- unique(sitecd)
  site_path <- paste0(data_path, "/")

  for (i in 1:length(unq_sites)) {

    print(paste("Processing data for site:", unq_sites[i]))

    c13_obs_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                             level = "dp01",
                                             var = "dlta13CCo2",
                                             avg = 9)
    c13_ref_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                             level = "dp01",
                                             var = "dlta13CCo2Refe",
                                             avg = 9)
    co2_obs_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                             level = "dp01",
                                             var = "rtioMoleDryCo2",
                                             avg = 9)
    co2_ref_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                             level = "dp01",
                                             var = "rtioMoleDryCo2Refe",
                                             avg = 9)

    #-----------------------------------------------------
    # select data a little more cleverly.

    #=========================
    # 1. CALIBRATION DATA.

    print("Extracting calibration data...")

    calData <- list()

    calData[[1]] <- c13_obs_data[[1]] %>%
      dplyr::filter(verticalPosition %in%
                      c("co2Low", "co2Med", "co2High", "co2Arch")) %>%
      dplyr::select(timeBgn, timeEnd,
                    data.isoCo2.dlta13CCo2.mean, verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd,
                    mean13C = data.isoCo2.dlta13CCo2.mean,
                    standard = verticalPosition)

    calData[[2]] <- c13_ref_data[[1]] %>%
      dplyr::filter(verticalPosition %in%
                      c("co2Low", "co2Med", "co2High", "co2Arch")) %>%
      dplyr::select(timeBgn, timeEnd,
                    data.isoCo2.dlta13CCo2Refe.mean, verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd,
                    ref13C = data.isoCo2.dlta13CCo2Refe.mean,
                    standard = verticalPosition)

    calData[[3]] <- co2_obs_data[[1]] %>%
      dplyr::filter(verticalPosition %in%
                      c("co2Low", "co2Med", "co2High", "co2Arch")) %>%
      dplyr::select(timeBgn, timeEnd,
                    data.isoCo2.rtioMoleDryCo2.mean, verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd,
                    meanCo2 = data.isoCo2.rtioMoleDryCo2.mean,
                    standard = verticalPosition)

    calData[[4]] <- co2_ref_data[[1]] %>%
      dplyr::filter(verticalPosition %in%
                      c("co2Low", "co2Med", "co2High", "co2Arch")) %>%
      dplyr::select(timeBgn, timeEnd,
                    data.isoCo2.rtioMoleDryCo2Refe.mean, verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd,
                    refCo2 = data.isoCo2.rtioMoleDryCo2Refe.mean,
                    standard = verticalPosition)

    calData <- Reduce(
      function(x, y) merge(x, y, by = c("timeBgn", "timeEnd", "standard")),
                            calData)

    # convert times to POSIXct.
    calData$timeBgn <- as.POSIXct(calData$timeBgn,
                                  format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    calData$timeEnd <- as.POSIXct(calData$timeEnd,
                                  format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")

    #=========================
    # 2. CALIBRATION PARAMETERS

    print("Extracting calibration parameters..")

    # need to get attributes for this site (i wonder if there's a better way?)
    flist <- list.files(paste0(site_path, unq_sites[i]), full.names = TRUE)

    calPars.tmp <- list()
    calPars <- list()

    for (k in 1:length(flist)) {

      calPars.tmp[[k]] <- rhdf5::h5read(flist[k],
                      paste0("/", unq_sites[i], "/dp01/data/isoCo2/calData"))

      calPars[[k]] <- calPars.tmp[[k]][[1]]

      # convert valid_period_start and valid_period_end to POSIXct.
      calPars[[k]]$valid_period_start <- as.POSIXct(calPars[[k]]$valid_period_start,
                                                    format = "%Y-%m-%dT%H:%M:%OSZ",
                                                    tz = "UTC")
      calPars[[k]]$valid_period_end <- as.POSIXct(calPars[[k]]$valid_period_end,
                                                  format = "%Y-%m-%dT%H:%M:%OSZ",
                                                  tz = "UTC")

    }

    # determine method.
    if (names(calPars.tmp[[1]]) == "calGainsOffsets") {
      method <- "Bowling"
    } else if (names(calPars.tmp[[1]]) == "calRegressions") {
      method <- "LinReg"
    } else {
      stop("Can't identify method.")
    }

    # save a monthly version for plotting scripts.
    calParsMon <- calPars

    # bind together before putting into plotting scripts.
    calPars <- do.call(rbind, calPars)

    #=========================
    # 3. AMBIENT DATA

    print("Extracting ambient data..")

    ambData <- list()

    attrs <- rhdf5::h5readAttributes(flist[1], unq_sites[i])
    heights <- as.numeric(attrs$DistZaxsLvlMeasTow)

    ambData[[1]] <- c13_obs_data[[1]] %>%
      dplyr::filter(verticalPosition %in%
               c("010", "020", "030", "040", "050", "060", "070", "080")) %>%
      dplyr::select(timeBgn, timeEnd,
                    data.isoCo2.dlta13CCo2.mean,
                    data.isoCo2.dlta13CCo2.mean_cal,
                    verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd,
                    mean13C = data.isoCo2.dlta13CCo2.mean_cal,
                    ucal13C = data.isoCo2.dlta13CCo2.mean,
                    level = verticalPosition)

    ambData[[2]] <- co2_obs_data[[1]] %>%
      dplyr::filter(verticalPosition %in%
               c("010", "020", "030", "040", "050", "060", "070", "080")) %>%
      dplyr::select(timeBgn, timeEnd, data.isoCo2.rtioMoleDryCo2.mean,
                    data.isoCo2.rtioMoleDryCo2.mean_cal, verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd,
                    meanCo2 = data.isoCo2.rtioMoleDryCo2.mean_cal,
                    ucalCo2 = data.isoCo2.rtioMoleDryCo2.mean,
                    level = verticalPosition)

    ambData <- Reduce(function(x, y)
      merge(x, y, by = c("timeBgn", "timeEnd", "level")), ambData)

    # loop through heights, and make a new vector that holds these heights.
    ambData$height <- 0

    for (j in 1:length(heights)) {
      level_var <- paste0("0", j, "0")

      ambData$height[ambData$level == level_var] <- heights[j]

    }

    # convert times to POSIXct.
    ambData$timeBgn <- as.POSIXct(ambData$timeBgn,
                                  format = "%Y-%m-%dT%H:%M:%OSZ",
                                  tz = "UTC")
    ambData$timeEnd <- as.POSIXct(ambData$timeEnd,
                                  format = "%Y-%m-%dT%H:%M:%OSZ",
                                  tz = "UTC")

    # 4. All data (only needed if which_plots includes variances.)
    
    print("Extracting variance data..")
    
    varData <- list()
    
    attrs <- rhdf5::h5readAttributes(flist[1], unq_sites[i])
    heights <- as.numeric(attrs$DistZaxsLvlMeasTow)
    
    varData[[1]] <- c13_obs_data[[1]] %>%
      dplyr::select(timeBgn, timeEnd,
                    data.isoCo2.dlta13CCo2.vari,
                    verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd,
                    vari13C = data.isoCo2.dlta13CCo2.vari,
                    level = verticalPosition)
    
    varData[[2]] <- co2_obs_data[[1]] %>%
      dplyr::select(timeBgn, timeEnd,
                    data.isoCo2.rtioMoleDryCo2.vari,
                    verticalPosition) %>%
      dplyr::rename(timeBgn = timeBgn, timeEnd = timeEnd,
                    variCo2 = data.isoCo2.rtioMoleDryCo2.vari,
                    level = verticalPosition)
    
    varData <- Reduce(function(x, y)
      merge(x, y, by = c("timeBgn", "timeEnd", "level")), varData)
    
    # convert times to POSIXct.
    varData$timeBgn <- as.POSIXct(varData$timeBgn,
                                  format = "%Y-%m-%dT%H:%M:%OSZ",
                                  tz = "UTC")
    varData$timeEnd <- as.POSIXct(varData$timeEnd,
                                  format = "%Y-%m-%dT%H:%M:%OSZ",
                                  tz = "UTC")
    
    #--------------------------------------------------------------
    #--------------------------------------------------------------
    # PLOTTING SCRIPTS LIVE BELOW.
    #--------------------------------------------------------------
    #--------------------------------------------------------------

    # 1. Raw calibration data - monthly
    if (which_plots == 1 | which_plots == 7 | which_plots == 9) {

      print("Plot 1")
      cplot_monthly_standards(calData,
                              out_folder,
                              unq_sites[i])

    }

    # 5. Calibration parameters - timeseries
    if (which_plots == 2 | which_plots == 7 | which_plots == 9) {

      print("Plot 2")
      cplot_monthly_calParameters(calParsMon,
                                  out_folder,
                                  unq_sites[i],
                                  method)

    } # if


    # 3. calibrated ambient data - timeseries
    if (which_plots == 3 | which_plots == 7 | which_plots == 9) {

      print("Plot 3")
      cplot_monthly_ambient(ambData,
                            out_folder,
                            unq_sites[i])

    } # if

    # 4. Raw calibration data - timeseries
    if (which_plots == 4 | which_plots == 8 | which_plots == 9) {

      print("Plot 4")
      cplot_fullts_standards(calData,
                             out_folder,
                             unq_sites[i])

    } # if

    # 5. Calibration parameters - timeseries
    if (which_plots == 5 | which_plots == 8 | which_plots == 9) {

      print("Plot 5")
      cplot_fullts_calParameters(calPars,
                                 out_folder,
                                 unq_sites[i],
                                 method)

    } # if

    # 6. calibrated ambient data - timeseries
    if (which_plots == 6 | which_plots == 8 | which_plots == 9) {

      print("Plot 6")
      cplot_fullts_ambient(ambData,
                           out_folder,
                           unq_sites[i])

    } # if

    # 6. standard distributions
    if (which_plots == 10 | which_plots == 8 | which_plots == 9) {

      print("Plot 10")
      cplot_standard_distributions(calData,
                                   out_folder,
                                   unq_sites[i])


    } # if

    # 7. timeseries of measurement variance - could help ID time issue.
    if (which_plots == 11 | which_plots == 9) {
      
      print("Plot 11")
      co2_variance_timeseries(varData,
                              out_folder,
                              unq_sites[i])
      
      
    } # if
    
    # 8. timeseries of measurement variance - could help ID time issue.
    if (which_plots == 12 | which_plots == 9) {
      
      print("Plot 12")
      c13_variance_timeseries(varData,
                              out_folder,
                              unq_sites[i])
      
      
    } # if
  } # for unq_sites
} # function
