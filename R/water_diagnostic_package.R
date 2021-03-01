#' water_diagnostic_package_bymonth.R
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data_path Provide path to where calibrated data from one site live.
#' @param which_sites Which NEON sites to run plots for? Default = all
#' @param plot_path Path to where output pdf plots should be written.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off pdf
#' @importFrom utils menu
#'
water_diagnostic_package_bymonth <- function(data_path,
                                      plot_path,
                                      which_sites = "all") {

  # what plots would be good here:
  # 1) timeseries of calibration data
  # 2) timeseries of calibration regressions
  # 3) timeseries of ambient data
  # 4) monthly versions of 1-3.
  # all of these should be queried when running this function.

  # query re: calibration plots
  print("This function makes diagnostic plots of NEON water isotope data.")

  #-------------------------------------------------------
  # query for which plots.
  which_plots <- menu(c("Raw Calibration data - Monthly",
                        "Calibration Parameters - Monthly",
                        "Atmospheric Measurements - Monthly",
                        "Raw Calibration data - Full Timeseries",
                        "Calibration Parametrs - Full Timeseries",
                        "Atmospheric Measurements - Full Timeseries",
                        "Calibration data inc. d-excess - Full Timeseries",
                        "Calibration data variance",
                        "All Monthly Plots",
                        "All Full Timeseries Plots",
                        "All Plots",
                        "I've made a huge mistake."),
                      title = "Which plots should be run?")

  # allow graceful exit if want to stop.
  if (which_plots == 12) {

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
    neon_sites <- water_isotope_sites()

    if (!(which_sites %in% neon_sites)) {

      stop("Invalid NEON site selected!")

    } else {

      slist <- list.files(paste0(data_path, "/", which_sites), pattern = ".h5")

    } # neon_sites

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

    o18_amb_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                             level = "dp01",
                                             var = "dlta18OH2o",
                                             avg = 9)
    h2_amb_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                            level = "dp01",
                                            var = "dlta2HH2o",
                                            avg = 9)
    h2o_amb_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                             level = "dp01",
                                             var = "rtioMoleWetH2o",
                                             avg = 9)
    o18_ref_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                             level = "dp01",
                                             var = "dlta18OH2oRefe",
                                             avg = 3)
    h2_ref_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                             level = "dp01",
                                             var = "dlta2HH2oRefe",
                                             avg = 3)
    o18_obs_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                             level = "dp01",
                                             var = "dlta18OH2o",
                                             avg = 3)
    h2_obs_data <- neonUtilities::stackEddy(paste0(site_path, unq_sites[i]),
                                            level = "dp01",
                                            var = "dlta2HH2o",
                                            avg = 3)

    #-----------------------------------------------------
    # select data a little more cleverly.

    #=========================
    # 1. CALIBRATION DATA.

    print("Extracting calibration data...")

    calData <- list()

    calData[[1]] <- o18_obs_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("h2oLow", "h2oMed", "h2oHigh")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta18OH2o.mean,
                    .data$data.isoH2o.dlta18OH2o.vari,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    mean18O = .data$data.isoH2o.dlta18OH2o.mean,
                    vari18O = .data$data.isoH2o.dlta18OH2o.vari,
                    standard = .data$verticalPosition)

    calData[[2]] <- o18_ref_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("h2oLow", "h2oMed", "h2oHigh")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta18OH2oRefe.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    ref18O = .data$data.isoH2o.dlta18OH2oRefe.mean,
                    standard = .data$verticalPosition)

    calData[[3]] <- h2_obs_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("h2oLow", "h2oMed", "h2oHigh")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta2HH2o.mean,
                    .data$data.isoH2o.dlta2HH2o.vari,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    mean2H = .data$data.isoH2o.dlta2HH2o.mean,
                    vari2H = .data$data.isoH2o.dlta2HH2o.vari,
                    standard = .data$verticalPosition)

    calData[[4]] <- h2_ref_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("h2oLow", "h2oMed", "h2oHigh")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta2HH2oRefe.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    ref2H = .data$data.isoH2o.dlta2HH2oRefe.mean,
                    standard = .data$verticalPosition)

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
                      paste0("/", unq_sites[i], "/dp01/data/isoH2o/calData"))

      calPars[[k]] <- calPars.tmp[[k]][[1]]

      # convert valid_period_start and valid_period_end to POSIXct.
      calPars[[k]]$valid_period_start <- as.POSIXct(calPars[[k]]$valid_period_start,
                                            format = "%Y-%m-%dT%H:%M:%OSZ",
                                            tz = "UTC")
      calPars[[k]]$valid_period_end <- as.POSIXct(calPars[[k]]$valid_period_end,
                                            format = "%Y-%m-%dT%H:%M:%OSZ",
                                            tz = "UTC")

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

    ambData[[1]] <- o18_amb_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
               c("010", "020", "030", "040", "050", "060", "070", "080")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta18OH2o.mean_cal,
                    .data$data.isoH2o.dlta18OH2o.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    mean18O = .data$data.isoH2o.dlta18OH2o.mean_cal,
                    ucal18O = .data$data.isoH2o.dlta18OH2o.mean,
                    level = .data$verticalPosition)

    ambData[[2]] <- h2_amb_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
               c("010", "020", "030", "040", "050", "060", "070", "080")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta2HH2o.mean_cal,
                    .data$data.isoH2o.dlta2HH2o.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    mean2H = .data$data.isoH2o.dlta2HH2o.mean_cal,
                    ucal2H = .data$data.isoH2o.dlta2HH2o.mean,
                    level = .data$verticalPosition)

    ambData[[3]] <- h2o_amb_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
               c("010", "020", "030", "040", "050", "060", "070", "080")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd, .data$data.isoH2o.rtioMoleWetH2o.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    meanH2o = .data$data.isoH2o.rtioMoleWetH2o.mean,
                    level = .data$verticalPosition)

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


    #--------------------------------------------------------------
    #--------------------------------------------------------------
    # PLOTTING SCRIPTS LIVE BELOW.
    #--------------------------------------------------------------
    #--------------------------------------------------------------

    # 1. Raw calibration data - monthly
    if (which_plots == 1 | which_plots == 9 | which_plots == 11) {

      print("Plot 1")
      wplot_monthly_standards(calData,
                              out_folder,
                              unq_sites[i])

    }

    # 2. Calibration parameters - monthly
    if (which_plots == 2 | which_plots == 9 | which_plots == 11) {

      print("Plot 2")
      wplot_monthly_calParameters(calParsMon,
                                  out_folder,
                                  unq_sites[i])

    } # if

    # 3. calibrated ambient data - monthly
    if (which_plots == 3 | which_plots == 9 | which_plots == 11) {

      print("Plot 3")
      wplot_monthly_ambient(ambData,
                            out_folder,
                            unq_sites[i])

    } # if

    # 4. Raw calibration data - timeseries
    if (which_plots == 4 | which_plots == 10 | which_plots == 11) {

      print("Plot 4")
      wplot_fullts_standards(calData,
                             out_folder,
                             unq_sites[i])

    } # if

    # 5. Calibration parameters - timeseries
    if (which_plots == 5 | which_plots == 10 | which_plots == 11) {

      print("Plot 5")
      wplot_fullts_calParameters(calPars,
                                 out_folder,
                                 unq_sites[i])

    } # if

    # 6. calibrated ambient data - timeseries
    if (which_plots == 6 | which_plots == 10 | which_plots == 11) {

      print("Plot 6")
      wplot_fullts_ambient(ambData,
                           out_folder,
                           unq_sites[i])

    } # if

    # 7. calibrated ambient data - w/ d-excess
    if (which_plots == 7 | which_plots == 10 | which_plots == 11) {

      print("Plot 7")
      wplot_fullts_dxsdiag(calData,
                           out_folder,
                           unq_sites[i])

    } # if
    
    # 8. calibrated ambient data - w/ variance
    if (which_plots == 8 | which_plots == 10 | which_plots == 11) {
      
      print("Plot 8")
      wplot_fullts_vari(calData,
                           out_folder,
                           unq_sites[i])
      
    } # if

  } # for unq_sites
} # function

#=======================================
#=======================================

#' water_diagnostic_package_bysite.R
#'
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param data_path Provide path to where calibrated data live.
#'       Assumes that the \code{calibrate_water_linreg_bysite} function has
#'       been used to calibrate these data, and there is one file per site.
#' @param which_sites Which NEON sites to run plots for? Default = all
#' @param plot_path Path to where output pdf plots should be written.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off pdf
#' @importFrom utils menu
#'
water_diagnostic_package_bysite <- function(data_path,
                                             plot_path,
                                             which_sites = "all") {
  
  # query re: calibration plots
  print("This function makes diagnostic plots of NEON water isotope data.")
  
  #-------------------------------------------------------
  # query for which plots.
  which_plots <- menu(c("Raw Calibration data - Monthly",
                        "Calibration Parameters - Monthly",
                        "Atmospheric Measurements - Monthly",
                        "Raw Calibration data - Full Timeseries",
                        "Calibration Parametrs - Full Timeseries",
                        "Atmospheric Measurements - Full Timeseries",
                        "Calibration data inc. d-excess - Full Timeseries",
                        "Calibration data variance",
                        "All Monthly Plots",
                        "All Full Timeseries Plots",
                        "All Plots",
                        "I've made a huge mistake."),
                      title = "Which plots should be run?")
  
  # allow graceful exit if want to stop.
  if (which_plots == 12) {
    
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
    
    slist <- list.files(data_path, pattern = ".h5")
    
  } else {
    
    # check to see if *is* a neon site.
    neon_sites <- water_isotope_sites()
    
    if (!(which_sites %in% neon_sites)) {
      
      stop("Invalid NEON site selected!")
      
    } else {
      
      slist <- list.files(data_path, pattern = ".h5")
      
    } # neon_sites
    
  } # which_sites "all"
  
  
  # extract lists of domains, site codes, and year-month combos from file names
  slist.tmp <- strsplit(slist, split = ".", fixed = TRUE)
  sitecd <- sapply(slist.tmp, "[[", 2)
  
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
  site_path <- data_path
  
  for (i in 1:length(unq_sites)) {
    
    print(paste("Processing data for site:", unq_sites[i]))
    
    fname <- paste0(site_path,'/',slist[grepl(unq_sites[i],slist)])
    
    o18_amb_data <- neonUtilities::stackEddy(fname,
                                             level = "dp01",
                                             var = "dlta18OH2o",
                                             avg = 9)
    h2_amb_data <- neonUtilities::stackEddy(fname,
                                            level = "dp01",
                                            var = "dlta2HH2o",
                                            avg = 9)
    h2o_amb_data <- neonUtilities::stackEddy(fname,
                                             level = "dp01",
                                             var = "rtioMoleWetH2o",
                                             avg = 9)
    o18_ref_data <- neonUtilities::stackEddy(fname,
                                             level = "dp01",
                                             var = "dlta18OH2oRefe",
                                             avg = 3)
    h2_ref_data <- neonUtilities::stackEddy(fname,
                                            level = "dp01",
                                            var = "dlta2HH2oRefe",
                                            avg = 3)
    o18_obs_data <- neonUtilities::stackEddy(fname,
                                             level = "dp01",
                                             var = "dlta18OH2o",
                                             avg = 3)
    h2_obs_data <- neonUtilities::stackEddy(fname,
                                            level = "dp01",
                                            var = "dlta2HH2o",
                                            avg = 3)

    #-----------------------------------------------------
    # select data a little more cleverly.
    
    #=========================
    # 1. CALIBRATION DATA.
    
    print("Extracting calibration data...")
    
    calData <- list()
    
    calData[[1]] <- o18_obs_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("h2oLow", "h2oMed", "h2oHigh")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta18OH2o.mean,
                    .data$data.isoH2o.dlta18OH2o.vari,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    mean18O = .data$data.isoH2o.dlta18OH2o.mean,
                    vari18O = .data$data.isoH2o.dlta18OH2o.vari,
                    standard = .data$verticalPosition)
    
    calData[[2]] <- o18_ref_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("h2oLow", "h2oMed", "h2oHigh")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta18OH2oRefe.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    ref18O = .data$data.isoH2o.dlta18OH2oRefe.mean,
                    standard = .data$verticalPosition)
    
    calData[[3]] <- h2_obs_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("h2oLow", "h2oMed", "h2oHigh")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta2HH2o.mean,
                    .data$data.isoH2o.dlta2HH2o.vari,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    mean2H = .data$data.isoH2o.dlta2HH2o.mean,
                    vari2H = .data$data.isoH2o.dlta2HH2o.vari,
                    standard = .data$verticalPosition)
    
    calData[[4]] <- h2_ref_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("h2oLow", "h2oMed", "h2oHigh")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta2HH2oRefe.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    ref2H = .data$data.isoH2o.dlta2HH2oRefe.mean,
                    standard = .data$verticalPosition)
    
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
    flist <- list.files(site_path, full.names = TRUE)
    flist <- flist[grepl(unq_sites[i],flist)]
    
    calPars.tmp <- list()
    calPars <- list()
  
    calPars.tmp <- rhdf5::h5read(flist,
                      paste0("/", unq_sites[i], "/dp01/data/isoH2o/calData"))
      
    calPars <- calPars.tmp[[1]]
      
    # convert valid_period_start and valid_period_end to POSIXct.
    calPars$valid_period_start <- as.POSIXct(calPars$valid_period_start,
                                             format = "%Y-%m-%dT%H:%M:%OSZ",
                                             tz = "UTC")
    calPars$valid_period_end <- as.POSIXct(calPars$valid_period_end,
                                           format = "%Y-%m-%dT%H:%M:%OSZ",
                                           tz = "UTC")

    # save a monthly version for plotting scripts.
    calParsMon <- split(calPars, format(calPars$valid_period_start, "%Y-%m"))
    
    #=========================
    # 3. AMBIENT DATA
    
    print("Extracting ambient data..")
    
    ambData <- list()
    
    attrs <- rhdf5::h5readAttributes(flist, unq_sites[i])
    heights <- as.numeric(attrs$DistZaxsLvlMeasTow)
    
    ambData[[1]] <- o18_amb_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("010", "020", "030", "040", "050", "060", "070", "080")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta18OH2o.mean_cal,
                    .data$data.isoH2o.dlta18OH2o.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    mean18O = .data$data.isoH2o.dlta18OH2o.mean_cal,
                    ucal18O = .data$data.isoH2o.dlta18OH2o.mean,
                    level = .data$verticalPosition)
    
    ambData[[2]] <- h2_amb_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("010", "020", "030", "040", "050", "060", "070", "080")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd,
                    .data$data.isoH2o.dlta2HH2o.mean_cal,
                    .data$data.isoH2o.dlta2HH2o.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    mean2H = .data$data.isoH2o.dlta2HH2o.mean_cal,
                    ucal2H = .data$data.isoH2o.dlta2HH2o.mean,
                    level = .data$verticalPosition)
    
    ambData[[3]] <- h2o_amb_data[[1]] %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("010", "020", "030", "040", "050", "060", "070", "080")) %>%
      dplyr::select(.data$timeBgn, .data$timeEnd, .data$data.isoH2o.rtioMoleWetH2o.mean,
                    .data$verticalPosition) %>%
      dplyr::rename(timeBgn = .data$timeBgn, timeEnd = .data$timeEnd,
                    meanH2o = .data$data.isoH2o.rtioMoleWetH2o.mean,
                    level = .data$verticalPosition)
    
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
    

    #--------------------------------------------------------------
    #--------------------------------------------------------------
    # PLOTTING SCRIPTS LIVE BELOW.
    #--------------------------------------------------------------
    #--------------------------------------------------------------
    
    # 1. Raw calibration data - monthly
    if (which_plots == 1 | which_plots == 9 | which_plots == 11) {
      
      print("Plot 1")
      wplot_monthly_standards(calData,
                              out_folder,
                              unq_sites[i])
      
    }
    
    # 2. Calibration parameters - monthly
    if (which_plots == 2 | which_plots == 9 | which_plots == 11) {
      
      print("Plot 2")
      wplot_monthly_calParameters(calParsMon,
                                  out_folder,
                                  unq_sites[i])
      
    } # if
    
    # 3. calibrated ambient data - monthly
    if (which_plots == 3 | which_plots == 9 | which_plots == 11) {
      
      print("Plot 3")
      wplot_monthly_ambient(ambData,
                            out_folder,
                            unq_sites[i])
      
    } # if
    
    # 4. Raw calibration data - timeseries
    if (which_plots == 4 | which_plots == 10 | which_plots == 11) {
      
      print("Plot 4")
      wplot_fullts_standards(calData,
                             out_folder,
                             unq_sites[i])
      
    } # if
    
    # 5. Calibration parameters - timeseries
    if (which_plots == 5 | which_plots == 10 | which_plots == 11) {
      
      print("Plot 5")
      wplot_fullts_calParameters(calPars,
                                 out_folder,
                                 unq_sites[i])
      
    } # if
    
    # 6. calibrated ambient data - timeseries
    if (which_plots == 6 | which_plots == 10 | which_plots == 11) {
      
      print("Plot 6")
      wplot_fullts_ambient(ambData,
                           out_folder,
                           unq_sites[i])
      
    } # if
    
    # 7. calibrated ambient data - w/ d-excess
    if (which_plots == 7 | which_plots == 10 | which_plots == 11) {
      
      print("Plot 7")
      wplot_fullts_dxsdiag(calData,
                           out_folder,
                           unq_sites[i])
      
    } # if
    
    # 8. calibrated ambient data - w/ variance
    if (which_plots == 8 | which_plots == 10 | which_plots == 11) {
      
      print("Plot 8")
      wplot_fullts_vari(calData,
                        out_folder,
                        unq_sites[i])
      
    } # if
    
  } # for unq_sites
} # function
