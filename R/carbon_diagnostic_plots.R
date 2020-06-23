#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#' @import ggplot2
#' @noRd
# carbon_diagnostic_plots
# 1. monthly plots of reference material measurements.
cplot_monthly_standards <- function(cal_data, plot_path, site) {

  # open plot.
  pdf(paste0(plot_path, "/", "1_monCStds_", site, ".pdf"))

  # break data into months.
  cal_data$standard <- as.character(cal_data$standard)
  cal_data$standard[cal_data$standard == "co2Low"] <- 1
  cal_data$standard[cal_data$standard == "co2Med"] <- 2
  cal_data$standard[cal_data$standard == "co2High"] <- 3

  cal_data$standard <- as.numeric(cal_data$standard)

  cal_data.xts <- xts::xts(cal_data[, 3:7], order.by = cal_data$timeBgn)
  cal_data.mon <- xts::split.xts(cal_data.xts, f = "months")

  for (i in 1:length(cal_data.mon)) {
    # set up plots.
    p1 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = mean13C, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C, obs")

    p2 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = ref13C, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C, ref")

    p3 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = mean13C - ref13C, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13Cdiff")

    p4 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = meanCo2, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("[CO2], obs")

    p5 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = refCo2, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("[CO2], ref")

    p6 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = meanCo2 - refCo2, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("[CO2]diff")

    # add plot to file.
    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 6, top = site)
  }

  dev.off()
}

#=================================================================
# 2. monthly plots of calibration parameters.

cplot_monthly_calParameters <- function(calParDf, plot_path, site, method) {

  # open plot.
  pdf(paste0(plot_path, "/", "2_monCPars_", site, "_", method, ".pdf"))

  # check method.
  if (method == "Bowling") {

    for (i in 1:length(calParDf)) {

      # need to plot: gain12C, gain13C, offset12C, offset13C.
      p1 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = gain12C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("gain, 12C")

      p2 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = gain13C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("gain, 13C")

      p3 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = offset12C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("offset, 12C")

      p4 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = offset13C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("offset, 13C")

      p5 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = r2_12C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("r2, 12C")

      p6 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = r2_13C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("r2, 13C")

      gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 6, top = site)

    }

  } else if (method == "LinReg") {

    for (i in 1:length(calParDf)) {

      # need to plot: slope, intercept, r2, calUcrt.
      p1 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = d13C_slope)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("d13C slope")

      p2 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = d13C_intercept)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("d13C intercept")

      p3 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = d13C_r2)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("d13C r2")

      p5 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = co2_slope)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("co2 slope")

      p6 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = co2_intercept)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("co2 intercept")

      p7 <- ggplot(data = calParDf[[i]],
                   aes(x = valid_period_start, y = co2_r2)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("co2 r2")

      gridExtra::grid.arrange(p1, p5, p2, p6, p3, p7, nrow = 3, top = site)
    } #i
  }# method

  dev.off()

}

#=================================================================
# 3. monthly plots of ambient measurements.
cplot_monthly_ambient <- function(amb_data, dir_plots, site) {

  print(str(amb_data))
  # get number of heights.
  heights <- sort(unique(amb_data$height))
  nheights <- length(heights)

  # drop level from amb_data
  amb_data <- amb_data %>%
    dplyr::select(-level)

  amb_data.xts <- xts::xts(amb_data[, c(3:7)], order.by = amb_data$timeBgn)

  amb_data.mon <- xts::split.xts(amb_data.xts, f = "months")

  # open plot.
  pdf(paste0(dir_plots, "/", "3_monCAmb_", site, ".pdf"))

  for (k in 1:length(amb_data.mon)) {

    for (j in 1:nheights) {

      amb_data_height <- subset(amb_data.mon[[k]], height == heights[j])

      # take out of xts format
      amb_data_df <- as.data.frame(cbind(zoo::index(amb_data_height),
                                        zoo::coredata(amb_data_height)))
      names(amb_data_df) <- c("timeBgn", "ucal13C", "mean13C",
                              "ucalCo2", "meanCo2", "height")

      # convert timeBgn back to posixct
      amb_data_df$timeBgn <- as.POSIXct(amb_data_df$timeBgn,
                                       origin = "1970-01-01")

      #make a plot of this data.
      if (2 * j - 1 < 10) {
        assign(paste0("p0", 2 * j - 1), {
          ggplot(data = amb_data_df) +
            geom_line(aes(x = timeBgn, y = ucal13C), col = "black") +
            geom_line(aes(x = timeBgn, y = mean13C), col = "red") +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })  
      } else {
        assign(paste0("p", 2 * j - 1), {
          ggplot(data = amb_data_df) +
            geom_line(aes(x = timeBgn, y = ucal13C), col = "black") +
            geom_line(aes(x = timeBgn, y = mean13C), col = "red") +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })  
      }

      if (2 * j < 10) {
        assign(paste0("p0", 2 * j), {
          ggplot(data = amb_data_df) +
            geom_line(aes(x = timeBgn, y = ucalCo2), col = "black") +
            geom_line(aes(x = timeBgn, y = meanCo2), col = "red") +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })
      } else {
        assign(paste0("p", 2 * j), {
          ggplot(data = amb_data_df) +
            geom_line(aes(x = timeBgn, y = ucalCo2), col = "black") +
            geom_line(aes(x = timeBgn, y = meanCo2), col = "red") +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })
      }

    } # j

    # generate list of grobs.
    plot.list <- ls(pattern = "^p")
    plots <- mget(plot.list)

    gridExtra::grid.arrange(grobs = plots, ncol = 2)

    rm(plots, plot.list, amb_data_height)
    rm(list = ls(pattern = "^p"))

  } # k

  dev.off()

}

#========================================================
# 4. monthly plots of reference material measurements.
cplot_fullts_standards <- function(cal_data, plot_path, site) {

  # open plot.
  pdf(paste0(plot_path, "/", "4_tsCStds_", site, ".pdf"))

  #   # set up plots.
  p1 <- ggplot(data = cal_data, aes(x = timeBgn, y = mean13C, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d13C, obs")

  p2 <- ggplot(data = cal_data, aes(x = timeBgn, y = ref13C, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d13C, ref")

  p3 <- ggplot(data = cal_data,
               aes(x = timeBgn, y = mean13C - ref13C, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d13Cdiff")

  p4 <- ggplot(data = cal_data, aes(x = timeBgn, y = meanCo2, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("[CO2], obs")

  p5 <- ggplot(data = cal_data, aes(x = timeBgn, y = refCo2, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("[CO2], ref")

  p6 <- ggplot(data = cal_data,
               aes(x = timeBgn, y = meanCo2 - refCo2, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("[CO2]diff")

  # add plot to file.
  gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 6, top = site)

  dev.off()

}

#=================================================================
# 5. timeseries plots of calibration parameters.

cplot_fullts_calParameters <- function(calParDf, plot_path, site, method) {

  # open plot.
  pdf(paste0(plot_path, "/", "5_tsCPars_", site, "_", method, ".pdf"))

  # check method.
  if (method == "Bowling") {

    # need to plot: gain12C, gain13C, offset12C, offset13C.
    p1 <- ggplot(data = calParDf, aes(x = valid_period_start, y = gain12C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("gain, 12C")

    p2 <- ggplot(data = calParDf, aes(x = valid_period_start, y = gain13C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("gain, 13C")

    p3 <- ggplot(data = calParDf, aes(x = valid_period_start, y = offset12C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("offset, 12C")

    p4 <- ggplot(data = calParDf, aes(x = valid_period_start, y = offset13C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("offset, 13C")

    p5 <- ggplot(data = calParDf, aes(x = valid_period_start, y = r2_12C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("r2, 12C")

    p6 <- ggplot(data = calParDf, aes(x = valid_period_start, y = r2_13C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("r2, 13C")

    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 6, top = site)

  } else if (method == "LinReg") {

    # need to plot: slope, intercept, r2, calUcrt.
    p1 <- ggplot(data = calParDf,
                 aes(x = valid_period_start, y = d13C_slope)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C slope")

    p2 <- ggplot(data = calParDf,
                 aes(x = valid_period_start, y = d13C_intercept)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C intercept")

    p3 <- ggplot(data = calParDf,
                 aes(x = valid_period_start, y = d13C_r2)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C r2")

    p5 <- ggplot(data = calParDf,
                 aes(x = valid_period_start, y = co2_slope)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("co2 slope")

    p6 <- ggplot(data = calParDf,
                 aes(x = valid_period_start, y = co2_intercept)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("co2 intercept")

    p7 <- ggplot(data = calParDf,
                 aes(x = valid_period_start, y = co2_r2)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("co2 r2")

    gridExtra::grid.arrange(p1, p5, p2, p6, p3, p7, nrow = 3, top = site)
  }

  dev.off()

}

#=================================================================
# 6. timeseries plots of ambient measurements.
cplot_fullts_ambient <- function(amb_data, dir_plots, site) {

  # get number of heights.
  heights <- sort(unique(amb_data$height))
  nheights <- length(heights)

  for (j in 1:nheights) {

    amb_data_height <- amb_data %>%
      dplyr::filter(height == heights[j])

    # make a plot of this data.
    if (2 * j - 1 < 10) {
      assign(paste0("p0", 2 * j - 1), {
        ggplot(data = amb_data_height) +
          geom_line(aes(x = timeBgn, y = ucal13C), col = "black") +
          geom_line(aes(x = timeBgn, y = mean13C), col = "red") +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })
    } else {
      assign(paste0("p", 2 * j - 1), {
        ggplot(data = amb_data_height) +
          geom_line(aes(x = timeBgn, y = ucal13C), col = "black") +
          geom_line(aes(x = timeBgn, y = mean13C), col = "red") +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })
    }

    if (2 * j < 10) {
      assign(paste0("p0", 2 * j), {
        ggplot(data = amb_data_height) +
          geom_line(aes(x = timeBgn, y = ucalCo2), col = "black") +
          geom_line(aes(x = timeBgn, y = meanCo2), col = "red") +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })
    } else { 
      assign(paste0("p", 2 * j), {
        ggplot(data = amb_data_height) +
          geom_line(aes(x = timeBgn, y = ucalCo2), col = "black") +
          geom_line(aes(x = timeBgn, y = meanCo2), col = "red") +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })}

  }

  # generate list of grobs.
  plot.list <- ls(pattern = "^p")
  plots <- mget(plot.list)

  # open plot.
  pdf(paste0(dir_plots, "/", "6_tsCAmb_", site, ".pdf"))
  gridExtra::grid.arrange(grobs = plots, ncol = 2, top = site)
  dev.off()

}

#=================================================================
# 7. empirical density functions of reference materials and isotopologues.

cplot_standard_distributions <- function(cal_data, plot_path, site) {

  # define f and R
  f <- 0.00474
  R <- 0.0111797

  # calculate 12CO2 and 13CO2 from d13C and CO2.
  if (!all(is.na(cal_data$refCo2)) & !all(is.na(cal_data$ref13C))) {

    cal_data$c12 <- (1 - f) * cal_data$refCo2 /
      (1 + R * (1 + cal_data$ref13C / 1000))

    cal_data$c13 <- (1 - f) * cal_data$refCo2 - cal_data$c12

  } else {
    cal_data$c12 <- NA
    cal_data$c13 <- NA
  }

  # make plots of the distribution of each variable.
  p1 <- ggplot(data = cal_data, aes(x = c12, y = ..density..)) +
    geom_histogram(binwidth = 1) +
    theme_bw() +
    scale_y_continuous("Frequency") +
    scale_x_continuous("12CO2 mixing ratio, ppm")

  p2 <- ggplot(data = cal_data, aes(x = c13, y = ..density..)) +
    geom_histogram(binwidth = 0.01) +
    theme_bw() +
    scale_y_continuous("Frequency") +
    scale_x_continuous("13CO2 mixing ratio, ppm")

  p3 <- ggplot(data = cal_data, aes(x = ref13C, y = ..density..)) +
    geom_histogram(binwidth = 0.1) +
    theme_bw() +
    scale_y_continuous("Frequency") +
    scale_x_continuous("d13C")

  p4 <- ggplot(data = cal_data, aes(x = refCo2, y = ..density..)) +
    geom_histogram(binwidth = 1) +
    theme_bw() +
    scale_y_continuous("Frequency") +
    scale_x_continuous("CO2 mixing ratio, ppm")

  # open plot.
  pdf(paste0(plot_path, "/", "7_refDist_", site, ".pdf"))
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, top = site)
  dev.off()

}
