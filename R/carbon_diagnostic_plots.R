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

  calData.xts <- xts(calData[,3:7],order.by=calData$timeBgn)
  calData.mon <- split.xts(calData.xts,f="months")
  
  for (i in 1:length(calData.mon)) {
    # set up plots.
    p1 <- ggplot(data=calData.mon[[i]],aes(x=index(calData.mon[[i]]),y=mean13C,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C, obs") 
    
    p2 <- ggplot(data=calData.mon[[i]],aes(x=index(calData.mon[[i]]),y=ref13C,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13C, ref")
    
    p3 <- ggplot(data=calData.mon[[i]],aes(x=index(calData.mon[[i]]),y=mean13C-ref13C,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("d13Cdiff")   
    
    p4 <- ggplot(data=calData.mon[[i]],aes(x=index(calData.mon[[i]]),y=meanCo2,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("[CO2], obs") 
    
    p5 <- ggplot(data=calData.mon[[i]],aes(x=index(calData.mon[[i]]),y=refCo2,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("[CO2], ref")
    
    p6 <- ggplot(data=calData.mon[[i]],aes(x=index(calData.mon[[i]]),y=meanCo2 - refCo2,col=factor(standard))) +
      geom_point() +
      theme_bw() +
      scale_x_datetime("date") +
      scale_y_continuous("[CO2]diff") 
    
    # add plot to file.
    grid.arrange(p1,p2,p3,p4,p5,p6,nrow=6)
  }
  
  dev.off()
}


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
  grid.arrange(p1,p2,p3,p4,p5,p6,nrow=6)
  
  dev.off()
  
}