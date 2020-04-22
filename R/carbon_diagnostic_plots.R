# carbon_diagnostic_plots

# 1. monthly plots of reference material measurements.
cplot_monthly_standards <- function(calData,plot_path,site) {
  
  # open plot.
  pdf(paste0(plot_path,"/","1_monCStds_",site,".pdf"))
  
  # break data into months.
  calData$standard[calData$standard == "co2Low"] <- 1
  calData$standard[calData$standard == "co2Med"] <- 2
  calData$standard[calData$standard == "co2High"] <- 3
  
  calData$standard <- as.numeric(calData$standard)

  calData.xts <- xts::xts(calData[,3:7],order.by=calData$timeBgn)
  calData.mon <- split.xts(calData.xts,f="months")
  
  for (i in 1:length(calData.mon)) {
    # set up plots.
    p1 <- ggplot(data=calData.mon[[i]],aes(x=zoo::index(calData.mon[[i]]),y=mean13C,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C, obs") 
    
    p2 <- ggplot(data=calData.mon[[i]],aes(x=zoo::index(calData.mon[[i]]),y=ref13C,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C, ref")
    
    p3 <- ggplot(data=calData.mon[[i]],aes(x=zoo::index(calData.mon[[i]]),y=mean13C-ref13C,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13Cdiff")   
    
    p4 <- ggplot(data=calData.mon[[i]],aes(x=zoo::index(calData.mon[[i]]),y=meanCo2,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("[CO2], obs") 
    
    p5 <- ggplot(data=calData.mon[[i]],aes(x=zoo::index(calData.mon[[i]]),y=refCo2,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("[CO2], ref")
    
    p6 <- ggplot(data=calData.mon[[i]],aes(x=zoo::index(calData.mon[[i]]),y=meanCo2 - refCo2,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("[CO2]diff") 
    
    # add plot to file.
    gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,nrow=6,top=site)
  }
  
  dev.off()
}

#=================================================================
# 2. monthly plots of calibration parameters.

cplot_monthly_calParameters <- function(calParDf,plot_path,site,method) {
  
  # open plot.
  pdf(paste0(plot_path,"/","2_monCPars_",site,"_",method,".pdf"))
  
  # check method.
  if (method == "Bowling") {
    
    for (i in 1:length(calParDf)) {
      
      # need to plot: gain12C, gain13C, offset12C, offset13C, calgood, and calUcrt.
      p1 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=gain12C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("gain, 12C") 
      
      p2 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=gain13C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("gain, 13C") 
      
      p3 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=offset12C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("offset, 12C") 
      
      p4 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=offset13C)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("offset, 13C") 
      
      p5 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=calUcrt)) +
        geom_point() +
        geom_hline(yintercept= -0.2,col="red",lty=2) +
        geom_hline(yintercept = 0.2,col="red",lty=2) +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("Uncertainty")
      
      p6 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=calgood)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("# good cal points") 
      
      gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,nrow=6,top=site)
      
    }
    
  } else if (method == "LinReg") {
    
    for (i in 1:length(calParDf)) {
      
      # need to plot: slope, intercept, r2, calUcrt.
      p1 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=d13C_slope)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("d13C slope") 
      
      p2 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=d13C_intercept)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("d13C intercept") 
      
      p3 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=d13C_r2)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("d13C r2") 
      
      p4 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=calDelUcrt)) +
        geom_point() +
        geom_hline(yintercept= -0.2,col="red",lty=2) +
        geom_hline(yintercept = 0.2,col="red",lty=2) +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("d13C uncertainty")
      
      p5 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=co2_slope)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("co2 slope") 
      
      p6 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=co2_intercept)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("co2 intercept") 
      
      p7 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=co2_r2)) +
        geom_point() +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("co2 r2") 
      
      p8 <- ggplot(data=calParDf[[i]],aes(x=valid_period_start,y=calCO2Ucrt)) +
        geom_point() +
        geom_hline(yintercept= -0.2,col="red",lty=2) +
        geom_hline(yintercept = 0.2,col="red",lty=2) +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("co2 uncertainty")

      gridExtra::grid.arrange(p1,p5,p2,p6,p3,p7,p4,p8,nrow=4,top=site)
      
    } #i
  }# method
  
  dev.off()
  
}

#=================================================================
# 3. monthly plots of ambient measurements.
cplot_monthly_ambient <- function(ambData,dir_plots,site) {
  
  # get number of heights.
  heights <- sort(unique(ambData$height))
  nheights <- length(heights)
  
  # drop level from ambData
  ambData <- ambData %>%
    dplyr::select(-level)
  
  ambData.xts <- xts::xts(ambData[,c(3:5)],order.by=ambData$timeBgn)
  
  ambData.mon <- split.xts(ambData.xts,f="months")
  
  # open plot.
  pdf(paste0(dir_plots,"/","3_monCAmb_",site,".pdf"))
  
  for (k in 1:length(ambData.mon)) {
    
    for (j in 1:nheights) {
      
      ambData_height <- subset(ambData.mon[[k]],height == heights[j])
      
      # take out of xts format because it's fing up.
      ambData_df <- as.data.frame(cbind(zoo::index(ambData_height),zoo::coredata(ambData_height)))
      names(ambData_df) <- c("timeBgn","mean13C","meanCo2","height")
      
      # convert timeBgn back to posixct *facepalm*
      ambData_df$timeBgn <- as.POSIXct(ambData_df$timeBgn,origin="1970-01-01")
      
      #make a plot of this data.
      assign(paste0("p",2*j-1),{ggplot(data=ambData_df,aes(x=timeBgn,y=mean13C)) +
          geom_line() +
          theme_bw() +
          scale_y_continuous(name=paste("Height:",heights[j],"m")) +
          scale_x_datetime(name="Time")})
      
      assign(paste0("p",2*j),{ggplot(data=ambData_df,aes(x=timeBgn,y=meanCo2)) +
          geom_line() +
          theme_bw() +
          scale_y_continuous(name=paste("Height:",heights[j],"m")) +
          scale_x_datetime(name="Time")})

    } # j
    
    # generate list of grobs.
    plot.list <- ls(pattern="^p")
    plots <- mget(plot.list)

    gridExtra::grid.arrange(grobs=plots,ncol=2)

    rm(plots,plot.list,ambData_height)
    rm(list=ls(pattern="^p"))
    
  } # k
  
  dev.off()
  
}

#========================================================
# 4. monthly plots of reference material measurements.
cplot_fullts_standards <- function(calData,plot_path,site) {
  
  # open plot.
  pdf(paste0(plot_path,"/","4_tsCStds_",site,".pdf"))
  
  #   # set up plots.
  p1 <- ggplot(data=calData,aes(x=timeBgn,y=mean13C,col=standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d13C, obs") 
  
  p2 <- ggplot(data=calData,aes(x=timeBgn,y=ref13C,col=standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d13C, ref")
  
  p3 <- ggplot(data=calData,aes(x=timeBgn,y=mean13C-ref13C,col=standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("d13Cdiff")   
  
  p4 <- ggplot(data=calData,aes(x=timeBgn,y=meanCo2,col=standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("[CO2], obs") 
  
  p5 <- ggplot(data=calData,aes(x=timeBgn,y=refCo2,col=standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("[CO2], ref")
  
  p6 <- ggplot(data=calData,aes(x=timeBgn,y=meanCo2 - refCo2,col=standard)) +
    geom_point() +
    theme_bw() +
    scale_x_datetime("date") +
    scale_y_continuous("[CO2]diff") 
  
  # add plot to file.
  gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,nrow=6,top=site)
  
  dev.off()
  
}

#=================================================================
# 5. timeseries plots of calibration parameters.

cplot_fullts_calParameters <- function(calParDf,plot_path,site,method) {
  
  # open plot.
  pdf(paste0(plot_path,"/","5_tsCPars_",site,"_",method,".pdf"))
  
  # check method.
  if (method == "Bowling") {
    
    # need to plot: gain12C, gain13C, offset12C, offset13C, calgood, and calUcrt.
    p1 <- ggplot(data=calParDf,aes(x=valid_period_start,y=gain12C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("gain, 12C") 
      
    p2 <- ggplot(data=calParDf,aes(x=valid_period_start,y=gain13C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("gain, 13C") 
    
    p3 <- ggplot(data=calParDf,aes(x=valid_period_start,y=offset12C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("offset, 12C") 
    
    p4 <- ggplot(data=calParDf,aes(x=valid_period_start,y=offset13C)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("offset, 13C") 
    
    p5 <- ggplot(data=calParDf,aes(x=valid_period_start,y=calUcrt)) +
      geom_point() +
      geom_hline(yintercept= -0.2,col="red",lty=2) +
      geom_hline(yintercept = 0.2,col="red",lty=2) +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("Uncertainty")
    
    p6 <- ggplot(data=calParDf,aes(x=valid_period_start,y=calgood)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("# good cal points") 
    
    gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,nrow=6,top=site)
    
  } else if (method == "LinReg") {
    
    # need to plot: slope, intercept, r2, calUcrt.
    p1 <- ggplot(data=calParDf,aes(x=valid_period_start,y=d13C_slope)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C slope") 
    
    p2 <- ggplot(data=calParDf,aes(x=valid_period_start,y=d13C_intercept)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C intercept") 
    
    p3 <- ggplot(data=calParDf,aes(x=valid_period_start,y=d13C_r2)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C r2") 
    
    p4 <- ggplot(data=calParDf,aes(x=valid_period_start,y=calDelUcrt)) +
      geom_point() +
      geom_hline(yintercept= -0.2,col="red",lty=2) +
      geom_hline(yintercept = 0.2,col="red",lty=2) +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C uncertainty")
    
    p5 <- ggplot(data=calParDf,aes(x=valid_period_start,y=co2_slope)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("co2 slope") 
    
    p6 <- ggplot(data=calParDf,aes(x=valid_period_start,y=co2_intercept)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("co2 intercept") 
    
    p7 <- ggplot(data=calParDf,aes(x=valid_period_start,y=co2_r2)) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("co2 r2") 
    
    p8 <- ggplot(data=calParDf,aes(x=valid_period_start,y=calCO2Ucrt)) +
      geom_point() +
      geom_hline(yintercept= -0.2,col="red",lty=2) +
      geom_hline(yintercept = 0.2,col="red",lty=2) +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("co2 uncertainty")
    
    gridExtra::grid.arrange(p1,p5,p2,p6,p3,p7,p4,p8,nrow=4,top=site)
  
  }
  
  dev.off()

}

#=================================================================
# 6. timeseries plots of ambient measurements.
cplot_fullts_ambient <- function(ambData,dir_plots,site) {

  # get number of heights.
  heights <- sort(unique(ambData$height))
  nheights <- length(heights)
  
  print(nheights)
  
  for (j in 1:nheights) {
  
    print(heights[j])

    ambData_height <- ambData %>%
      dplyr::filter(height == heights[j])
    
    # make a plot of this data.
    assign(paste0("p",2*j-1),{ggplot(data=ambData_height,aes(x=timeBgn,y=mean13C)) + 
        geom_line() + 
        theme_bw() +
        scale_y_continuous(name=paste("Height:",heights[j],"m")) +
        scale_x_datetime(name="Time")})

    assign(paste0("p",2*j),{ggplot(data=ambData_height,aes(x=timeBgn,y=meanCo2)) + 
        geom_line() + 
        theme_bw() +
        scale_y_continuous(name=paste("Height:",heights[j],"m")) +
        scale_x_datetime(name="Time")})
    
  }
  
  # generate list of grobs.
  plot.list <- ls(pattern="^p")
  plots <- mget(plot.list)
  
  # open plot.
  pdf(paste0(dir_plots,"/","6_tsCAmb_",site,".pdf"))
  gridExtra::grid.arrange(grobs=plots,ncol=2,top=site)
  dev.off()
  
}
