#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#' @import ggplot2
#' @noRd
# water_diagnostic_plots
# 1. monthly plots of reference material measurements.
wplot_monthly_standards <- function(cal_data, plot_path, site) {

  # open plot.
  pdf(paste0(plot_path, "/", "1_monWStds_", site, ".pdf"))

  # break data into months.
  cal_data$standard <- as.numeric(cal_data$standard)

  cal_data.xts <- xts::xts(cal_data[, 3:7], order.by = cal_data$timeBgn)
  cal_data.mon <- xts::split.xts(cal_data.xts, f = "months")

  for (i in 1:length(cal_data.mon)) {
    # set up plots.
    p1 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = mean18O, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date", date_labels = "%m-%d") +
      scale_y_continuous("d18O, obs")

    p2 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = ref18O, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date", date_labels = "%m-%d") +
      scale_y_continuous("d18O, ref")

    p3 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = mean18O - ref18O, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date", date_labels = "%m-%d") +
      scale_y_continuous("d18Odiff")

    p4 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = mean2H, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date", date_labels = "%m-%d") +
      scale_y_continuous("d2H, obs")

    p5 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = ref2H, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date", date_labels = "%m-%d") +
      scale_y_continuous("d2H, ref")

    p6 <- ggplot(data = cal_data.mon[[i]],
                 aes(x = zoo::index(cal_data.mon[[i]]),
                     y = mean2H - ref2H, col = factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date", date_labels = "%m-%d") +
      scale_y_continuous("d2H, diff")

    # add plot to file.
    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 6,
      top = paste0(site, " - ",
                   lubridate::month(zoo::index(cal_data.mon[[i]])[1]),
                   "-",
                   lubridate::year(zoo::index(cal_data.mon[[i]])[1])))
  }

  dev.off()
}

#=================================================================
# 2. monthly plots of calibration parameters.

wplot_monthly_calParameters <- function(calParDf, plot_path, site) {

  # open plot.
  pdf(paste0(plot_path, "/", "2_monWPars_", site, ".pdf"))

  # loop through months
  for (i in 1:length(calParDf)) {
    
    # need to plot: slope, intercept, r2, calUcrt.
    p1 <- ggplot(data = calParDf[[i]],
                 aes(x = valid_period_start, y = o_slope)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d18O slope")
    
    p2 <- ggplot(data = calParDf[[i]],
                 aes(x = valid_period_start, y = o_intercept)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d18O intercept")
    
    p3 <- ggplot(data = calParDf[[i]],
                 aes(x = valid_period_start, y = o_r2)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d18O r2")
    
    p4 <- ggplot(data = calParDf[[i]],
                 aes(x = valid_period_start, y = h_slope)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d2H slope")
    
    p5 <- ggplot(data = calParDf[[i]],
                 aes(x = valid_period_start, y = h_intercept)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d2H intercept")
    
    p6 <- ggplot(data = calParDf[[i]],
                 aes(x = valid_period_start, y = h_r2)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d2H r2")
    
    gridExtra::grid.arrange(p1, p4, p2, p5, p3, p6, nrow = 3,
            top = paste0(site, " - ",
                         lubridate::month(as.POSIXct(calParDf[[i]]$valid_period_start[1],
                                                     origin = "1970-01-01")),
                         "-",
                         lubridate::year(as.POSIXct(calParDf[[i]]$valid_period_start[1],
                                                    origin = "1970-01-01"))))
  } #i

  dev.off()

}

#=================================================================
# 3. monthly plots of ambient measurements.
wplot_monthly_ambient <- function(amb_data, dir_plots, site) {

  # get number of heights.
  heights <- sort(unique(amb_data$height))
  nheights <- length(heights)

  # drop level from amb_data
  amb_data <- amb_data %>%
    dplyr::select(-level)

  amb_data.xts <- xts::xts(amb_data[, 3:8], order.by = amb_data$timeBgn)

  amb_data.mon <- xts::split.xts(amb_data.xts, f = "months")

  # open plot.
  pdf(paste0(dir_plots, "/", "3_monWAmb_", site, ".pdf"))

  for (k in 1:length(amb_data.mon)) {

    for (j in 1:nheights) {

      amb_data_height <- subset(amb_data.mon[[k]], height == heights[j])

      # take out of xts format
      amb_data_df <- as.data.frame(cbind(zoo::index(amb_data_height),
                                        zoo::coredata(amb_data_height)))
      names(amb_data_df) <- c("timeBgn", "mean18O", "ucal18O", "mean2H", "ucal2H", "meanH2o", "height")

      # convert timeBgn back to posixct
      amb_data_df$timeBgn <- as.POSIXct(amb_data_df$timeBgn,
                                       origin = "1970-01-01")

      #make a plot of this data.
      if (3 * j - 2 < 10) {
        assign(paste0("p0", 3 * j - 2), {
          ggplot(data = amb_data_df) +
            geom_line(aes(x = timeBgn, y = ucal18O), col = 'black') +
            geom_line(aes(x = timeBgn, y = mean18O), col = 'red') +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })
      } else {
        assign(paste0("p", 3 * j - 2), {
          ggplot(data = amb_data_df) +
            geom_line(aes(x = timeBgn, y = ucal18O), col = 'black') +
            geom_line(aes(x = timeBgn, y = mean18O), col = 'red') +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })
      }

      if (3 * j - 1 < 10) {
        assign(paste0("p0", 3 * j - 1), {
          ggplot(data = amb_data_df) +
            geom_line(aes(x = timeBgn, y = ucal2H), col = 'black') +
            geom_line(aes(x = timeBgn, y = mean2H), col = 'red') +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })
      } else {
        assign(paste0("p", 3 * j - 1), {
          ggplot(data = amb_data_df) +
            geom_line(aes(x = timeBgn, y = ucal2H), col = 'black') +
            geom_line(aes(x = timeBgn, y = mean2H), col = 'red') +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })
      }

      if (3 * j < 10) {
        assign(paste0("p0", 3 * j), {
          ggplot(data = amb_data_df, aes(x = timeBgn, y = meanH2o)) +
            geom_line() +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })
      } else {
        assign(paste0("p", 3 * j), {
          ggplot(data = amb_data_df, aes(x = timeBgn, y = meanH2o)) +
            geom_line() +
            theme_bw() +
            scale_y_continuous(name = paste("Height:", heights[j], "m")) +
            scale_x_datetime(name = "Time")
        })
      }

    } # j

    # generate list of grobs.
    plot.list <- ls(pattern = "^p")
    plots <- mget(plot.list)

    gridExtra::grid.arrange(grobs = plots, ncol = 3,
               top = paste0(site, " - ",
                            lubridate::month(as.POSIXct(amb_data_df$timeBgn[1], origin = "1970-01-01")),
                            "-",
                            lubridate::year(as.POSIXct(amb_data_df$timeBgn[1], origin = "1970-01-01"))))

    rm(plots, plot.list, amb_data_height)
    rm(list = ls(pattern = "^p"))

  } # k

  dev.off()

}

#========================================================
# 4. monthly plots of reference material measurements.
wplot_fullts_standards <- function(cal_data, plot_path, site) {

  # open plot.
  pdf(paste0(plot_path, "/", "4_tsWStds_", site, ".pdf"))

  #   # set up plots.
  p1 <- ggplot(data = cal_data, aes(x = timeBgn, y = mean18O, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d18O, obs")

  p2 <- ggplot(data = cal_data, aes(x = timeBgn, y = ref18O, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d18O, ref")

  p3 <- ggplot(data = cal_data,
               aes(x = timeBgn, y = mean18O - ref18O, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d18O, diff")

  p4 <- ggplot(data = cal_data, aes(x = timeBgn, y = mean2H, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d2H, obs")

  p5 <- ggplot(data = cal_data, aes(x = timeBgn, y = ref2H, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d2H, ref")

  p6 <- ggplot(data = cal_data,
               aes(x = timeBgn, y = mean2H - ref2H, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d2H, diff")

  # add plot to file.
  gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 6, top = site)

  dev.off()

}

#=================================================================
# 5. timeseries plots of calibration parameters.

wplot_fullts_calParameters <- function(calParDf, plot_path, site) {

  # open plot.
  pdf(paste0(plot_path, "/", "5_tsWPars_", site, ".pdf"))

  # plot data
  p1 <- ggplot(data = calParDf,
               aes(x = valid_period_start, y = o_slope)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d18O slope")

  p2 <- ggplot(data = calParDf,
               aes(x = valid_period_start, y = o_intercept)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d18O intercept")

  p3 <- ggplot(data = calParDf,
               aes(x = valid_period_start, y = o_r2)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d18O r2")

  p5 <- ggplot(data = calParDf,
               aes(x = valid_period_start, y = h_slope)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d2H slope")

  p6 <- ggplot(data = calParDf,
               aes(x = valid_period_start, y = h_intercept)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d2H intercept")

  p7 <- ggplot(data = calParDf,
               aes(x = valid_period_start, y = h_r2)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d2H r2")
  
  gridExtra::grid.arrange(p1, p5, p2, p6, p3, p7, nrow = 3, top = site)

  dev.off()

}

#=================================================================
# 6. timeseries plots of ambient measurements.
wplot_fullts_ambient <- function(amb_data, dir_plots, site) {

  # get number of heights.
  heights <- sort(unique(amb_data$height))
  nheights <- length(heights)

  for (j in 1:nheights) {

    amb_data_height <- amb_data %>%
      dplyr::filter(height == heights[j])

    #make a plot of this data.
    if (3 * j - 2 < 10) {
      assign(paste0("p0", 3 * j - 2), {
        ggplot(data = amb_data_height) +
          geom_line(aes(x = timeBgn, y = ucal18O), col = 'black') +
          geom_line(aes(x = timeBgn, y = mean18O), col = 'red') +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })
    } else {
      assign(paste0("p", 3 * j - 2), {
        ggplot(data = amb_data_height) +
          geom_line(aes(x = timeBgn, y = ucal18O), col = 'black') +
          geom_line(aes(x = timeBgn, y = mean18O), col = 'red') +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })
    }

    if (3 * j - 1 < 10) {
      assign(paste0("p0", 3 * j - 1), {
        ggplot(data = amb_data_height) +
          geom_line(aes(x = timeBgn, y = ucal2H), col = 'black') +
          geom_line(aes(x = timeBgn, y = mean2H), col = 'red') +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })
    } else {
      assign(paste0("p", 3 * j - 1), {
        ggplot(data = amb_data_height) +
          geom_line(aes(x = timeBgn, y = ucal2H), col = 'black') +
          geom_line(aes(x = timeBgn, y = mean2H), col = 'red') +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })
    }

    if (3 * j < 10) {
      assign(paste0("p0", 3 * j), {
        ggplot(data = amb_data_height, aes(x = timeBgn, y = meanH2o)) +
          geom_line() +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })
    } else {
      assign(paste0("p", 3 * j), {
        ggplot(data = amb_data_height, aes(x = timeBgn, y = meanH2o)) +
          geom_line() +
          theme_bw() +
          scale_y_continuous(name = paste("Height:", heights[j], "m")) +
          scale_x_datetime(name = "Time")
      })
    }

  }

  # generate list of grobs.
  plot_list <- ls(pattern = "^p")
  plots <- mget(plot_list)

  # open plot.
  pdf(paste0(dir_plots, "/", "6_tsWAmb_", site, ".pdf"))
  gridExtra::grid.arrange(grobs = plots, ncol = 3, top = site)
  dev.off()
  rm(list = ls(pattern = "^p"))

}

#========================================================
# 7. monthly plots of reference material measurements.
wplot_fullts_dxsdiag <- function(cal_data, plot_path, site) {

  # open plot.
  pdf(paste0(plot_path, "/", "7_tsWStdsDxs_", site, ".pdf"))

  #   # set up plots.
  p1 <- ggplot(data = cal_data, aes(x = timeBgn, y = mean18O, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d18O, obs")

  p2 <- ggplot(data = cal_data, aes(x = timeBgn, y = ref18O, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d18O, ref")

  p3 <- ggplot(data = cal_data, aes(x = timeBgn, y = mean2H, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d2H, obs")

  p4 <- ggplot(data = cal_data, aes(x = timeBgn, y = ref2H, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d2H, ref")

  p5 <- ggplot(data = cal_data,
               aes(x = timeBgn, y = mean2H - 8*mean18O, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("obs d-excess")

  p6 <- ggplot(data = cal_data,
               aes(x = timeBgn, y = ref2H - 8*ref18O, col = standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("ref d-excess")

  # add plot to file.
  gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 6, top = site)
  
  dev.off()
  
}

