#' Title
#'
#' @param data_path 
#' @param plot_path 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
carbon_diagnostic_package <- function(data_path,plot_path,method) {
  
  # what plots would be good here:
  # 1) timeseries of calibration data
  # 2) timeseries of calibratino regressions
  # 3) timeseries of ambient data
  # 4) monthly versions of 1-3.
  # all of these should be queried when running this function.
  
  # query re: calibration plots
  print("This function makes diagnostic plots of calibrated NEON carbon isotope data.")
  
  which.plots <- menu(c("Raw Calibration data - Monthly","Calibration Parameters - Monthly","Atmospheric Measurements - Monthly",
                        "Raw Calibration data - Full Timeseries","Calibration Parametrs - Full Timeseries","Atmospheric Measurements - Full Timeseries",
                        "All Monthly Plots","All Full Timeseries Plots","All Plots","I've made a huge mistake.")
                      ,title="Which plots should be run?")
  
  # allow graceful exit if want to stop.
  if (which.plots == 10) {
    stop()
  }
  
  #--------------------------------------------------------------
  # Set up logical tests to determine which plots below to run.
  
  
  
  #--------------------------------------------------------------
  #--------------------------------------------------------------
  # PLOTTING SCRIPTS LIVE BELOW.
  #--------------------------------------------------------------
  #--------------------------------------------------------------
  
  # 1. Raw calibration data - monthly
  # note: REPLACE WONKY STEPS HERE W/ STACK EDDY.
  for (i in 1:length(codes)) {
    
    print(codes[i])
    # find the files associated w/ that site.
    slist <- list.files(paste0(data.path,codes[i]),
                        pattern='.h5',
                        full.names=TRUE,
                        recursive=TRUE)
    
    # get site month/year for plot title.
    slist.tmp <- strsplit(slist,split="/",fixed=TRUE)
    yrmn <- sapply(slist.tmp,'[[',8)
    rm(slist.tmp)
    
    # make panel plot! one file per site...
    pdf(paste0("~/Dropbox/NEON_plots/200129/calVal/",codes[i],"_standards_carbon.pdf"))
    
    # start increment counters
    wcount <- 0
    ccount <- 0
    
    # make a plot for each month w/in that site.
    for (j in 1:length(slist)) {
      print(slist[j])
      #-------------------------------------------------------------
      # CARBON
      #-------------------------------------------------------------
      # open h5 file.
      cdata <- h5read(slist[j],paste0('/',codes[i],'/dp01/data/isoCo2/'))
      
      # read in all the dataframes we'll neede here:
      # 4 parameters of interest for carbon:
      # observed and reference isotope ratios
      # observed and reference concentrations
      
      # observed carbon isotope ratio
      rolow <- cdata[['co2Low_09m']][['dlta13CCo2']]
      romed <- cdata[['co2Med_09m']][['dlta13CCo2']]
      rohigh <- cdata[['co2High_09m']][['dlta13CCo2']]
      
      # reference carbon isotope ratio
      rrlow <- cdata[['co2Low_09m']][['dlta13CCo2Refe']]
      rrmed <- cdata[['co2Med_09m']][['dlta13CCo2Refe']]
      rrhigh <- cdata[['co2High_09m']][['dlta13CCo2Refe']]
      
      # observed co2 mixing ratio
      colow <- cdata[['co2Low_09m']][['rtioMoleDryCo2']]
      comed <- cdata[['co2Med_09m']][['rtioMoleDryCo2']]
      cohigh <- cdata[['co2High_09m']][['rtioMoleDryCo2']]
      
      # reference co2 mixing ratio
      crlow <- cdata[['co2Low_09m']][['rtioMoleDryCo2Refe']]
      crmed <- cdata[['co2Med_09m']][['rtioMoleDryCo2Refe']]
      crhigh <- cdata[['co2High_09m']][['rtioMoleDryCo2Refe']]
      
      # restructure, clean up.
      rm(cdata)
      
      # add treatment for months where no data are collected,
      # to prevent script from crashing.
      #=====================================================
      # low standard
      if (!is.null(rolow)) {
        rolow <- rolow %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "low")
        rrlow <- rrlow %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "low")
        colow <- colow %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "low")
        crlow <- crlow %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "low")
        ccount <- ccount + 1
      } else {
        rolow <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="low")
        rrlow <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="low")
        colow <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="low")
        crlow <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="low")
      }
      
      # medium standard
      if (!is.null(romed)) {
        romed <- romed %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "med")
        rrmed <- rrmed %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "med")
        comed <- comed %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "med")
        crmed <- crmed %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "med")
        ccount <- ccount + 1
      } else {
        romed <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="med")
        rrmed <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="med")
        comed <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="med")
        crmed <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="med")
      }
      
      # high standards
      if (!is.null(rohigh)) {
        rohigh <- rohigh %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "high")
        rrhigh <- rrhigh %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "high")
        cohigh <- cohigh %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "high")
        crhigh <- crhigh %>%
          select(mean,vari,timeBgn) %>%
          mutate(standard = "high")
        ccount <- ccount + 1
      } else {
        rohigh <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="high")
        rrhigh <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="high")
        cohigh <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="high")
        crhigh <- data.frame(mean=as.numeric(NA),timeBgn=as.POSIXct('1970-01-01'),vari=as.numeric(NA),standard="high")
      }
      
      # fix time variables for all 12 plot vars.
      rolow$timeBgn <- as.POSIXct(colow$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      rrlow$timeBgn <- as.POSIXct(crlow$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      colow$timeBgn <- as.POSIXct(colow$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      crlow$timeBgn <- as.POSIXct(crlow$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      
      romed$timeBgn <- as.POSIXct(comed$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      rrmed$timeBgn <- as.POSIXct(crmed$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      comed$timeBgn <- as.POSIXct(comed$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      crmed$timeBgn <- as.POSIXct(crmed$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      
      rohigh$timeBgn <- as.POSIXct(cohigh$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      rrhigh$timeBgn <- as.POSIXct(crhigh$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      cohigh$timeBgn <- as.POSIXct(cohigh$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      crhigh$timeBgn <- as.POSIXct(crhigh$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      
      # add quality flags from the dlta13CCo2 qfqm.
      qdata <- h5read(slist[j],paste0('/',codes[i],'/dp01/qfqm/isoCo2/'))
      
      qlow <- qdata[['co2Low_09m']][['dlta13CCo2']]
      qmed <- qdata[['co2Med_09m']][['dlta13CCo2']]
      qhigh <- qdata[['co2High_09m']][['dlta13CCo2']]
      
      # add quality flags to all variables.
      rolow$qual <- rrlow$qual <- colow$qual <- crlow$qual <- qlow$qfFinl
      romed$qual <- rrmed$qual <- comed$qual <- crmed$qual <- qmed$qfFinl
      rohigh$qual <- rrhigh$qual <- cohigh$qual <- crhigh$qual <- qhigh$qfFinl
      
      # bind together.
      rostds <- do.call(rbind,list(rolow,romed,rohigh))
      rrstds <- do.call(rbind,list(rrlow,rrmed,rrhigh))
      costds <- do.call(rbind,list(colow,comed,cohigh))
      crstds <- do.call(rbind,list(crlow,crmed,crhigh))
      
      # clean up again.
      rm(rolow)
      rm(rrlow)
      rm(colow)
      rm(cclow)
      
      rm(romed)
      rm(rrmed)
      rm(comed)
      rm(ccmed)
      
      rm(rohigh)
      rm(rrhigh)
      rm(cohigh)
      rm(cchigh)
      
      rm(qlow)
      rm(qmed)
      rm(qhigh)
      
      # set up plots.
      p1 <- ggplot(data=rostds,aes(x=timeBgn,y=mean,col=standard,shape=factor(qual))) +
        geom_point() +
        geom_errorbar(aes(ymin=mean-sqrt(vari),ymax=mean+sqrt(vari))) +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("d13C, measured")
      
      p2 <- ggplot(data=rrstds,aes(x=timeBgn,y=mean,col=standard,shape=factor(qual))) +
        geom_point() +
        geom_errorbar(aes(ymin=mean-sqrt(vari),ymax=mean+sqrt(vari))) +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("d13C, reference")
      
      p3 <- ggplot(data=costds,aes(x=timeBgn,y=mean,col=standard,shape=factor(qual))) +
        geom_point() +
        geom_errorbar(aes(ymin=mean-sqrt(vari),ymax=mean+sqrt(vari))) +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("[CO2], measured")
      
      p4 <- ggplot(data=crstds,aes(x=timeBgn,y=mean,col=standard,shape=factor(qual))) +
        geom_point() +
        geom_errorbar(aes(ymin=mean-sqrt(vari),ymax=mean+sqrt(vari))) +
        theme_bw() +
        scale_x_datetime("date") +
        scale_y_continuous("[CO2], reference")
      
      # add plot to file.
      grid.arrange(p1,p2,p3,p4,nrow=4,top=paste(codes[i],names[i],domain.number[i],domain.name[i],yrmn[j]))
      
      # mystical salt circle to protect us from evil hdf5 spirits.
      h5closeAll()
      
    }
    # close plot device for that site.
    dev.off()
    
    # save information about each site to a data frame.
    print(c(codes[i],length(slist),ccount,wcount))
    
    site.nmon[i,1] <- codes[i]
    site.nmon[i,2] <- length(slist)
    site.nmon[i,3] <- ccount
    site.nmon[i,4] <- wcount
  
}