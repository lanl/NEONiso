#' process_carbon_calibration_data
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param fname 
#' @param site 
#' @param time.diff.betweeen.standards 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
#' 

process_carbon_calibration_data <- function(fname,site,time.diff.betweeen.standards=1800,method=1){
  

  # print status.
  print("Processing carbon calibration data...")
  
  #=======================================================================
  # which method is being applied?
  if (method==1) {
    print("Applying method 1: two-point mixing ratio gain and offset calibration")
    print("Uses third point as calibration verification.")
    print("Reference: Bowling et al. 2003 AFM")
  } else if (method == 2) {
    print("Applying method 2: three-point mixing ratio bracketing interpolation")
  } else {
    stop("Invalid method selected.")
  }
  
  ciso <- h5read(fname,paste0('/',site,'/dp01/data/isoCo2'))
  
  # extract standards data.
  high <- ciso$co2High_09m
  med <- ciso$co2Med_09m
  low <- ciso$co2Low_09m
  
  # OKAY, what method are we applying???
  if (method == 1) {
    
    #-----------------------------------------------------
    # Method 1 follows the Bowling et al. 2003 AFM approach
    # where each isotopologue is calibrated individually.
    
    # Need to specify a few parameters for this method...
    #-----------------------------------------------------
    f <- 0.00474  		# fraction of CO2 isotopomers that aren't 12CO2 or 13CO2
    # note: f technically varies, but this has little impact
    # on calibration per Griffis et al. 2004.
    R_vpdb <- 0.0111797	# 13C/12C ratio of VPDB
    
    # set up and manipulate data frames from standards data.
    # For carbon, high/medium/low standards seem to be sorted
    # by CO2 concentration, and not (necessarily) by isotope ratio.
    #---------------------------------------------------------
    # "high" standard
    high_rs <- data.frame(d13C_meas_mean=high$dlta13CCo2$mean,d13C_meas_var=high$dlta13CCo2$vari,
                          CO2_meas_conc=high$rtioMoleDryCo2$mean,d13C_meas_n=high$dlta13CCo2$numSamp,
                          d13C_meas_btime=high$dlta13CCo2$timeBgn,d13C_meas_etime=high$dlta13CCo2$timeEnd,
                          d13C_ref_mean=high$dlta13CCo2Refe$mean,d13C_ref_var=high$dlta13CCo2Refe$vari,
                          d13C_ref_n=high$dlta13CCo2Refe$numSamp,d13C_ref_btime=high$dlta13CCo2Refe$timeBgn,
                          d13C_ref_etime=high$dlta13CCo2Refe$timeEnd,CO2_ref_conc=high$rtioMoleDryCo2Refe$mean,
                          conc12CCO2_obs=high$rtioMoleDry12CCo2$mean,conc13CCO2_obs=high$rtioMoleDry13CCo2$mean)
    
    # calculate 12CO2 and 13CO2 concentrations for high standard
    # for reference and observed isotope ratios
    high_rs <- high_rs %>%
      mutate(std_name="high") %>%
      mutate(conc12CCO2_ref = CO2_ref_conc*(1-0.00474)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
      mutate(conc13CCO2_ref = CO2_ref_conc*(1-0.00474)-conc12CCO2_ref) %>%
      #mutate(conc12CCO2_obs = CO2_meas_conc*(1-0.00474)/(1+R_vpdb*(1+d13C_meas_mean/1000))) %>%
      #mutate(conc13CCO2_obs = CO2_meas_conc*(1-0.00474)-conc12CCO2_obs) %>%
      mutate(d13C_meas_btime=as.POSIXct(d13C_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")) # for assigning times later.
    
    
    # "medium" standard
    med_rs <- data.frame(d13C_meas_mean=med$dlta13CCo2$mean,d13C_meas_var=med$dlta13CCo2$vari,
                         CO2_meas_conc=med$rtioMoleDryCo2$mean,d13C_meas_n=med$dlta13CCo2$numSamp,
                         d13C_meas_btime=med$dlta13CCo2$timeBgn,d13C_meas_etime=med$dlta13CCo2$timeEnd,
                         d13C_ref_mean=med$dlta13CCo2Refe$mean,d13C_ref_var=med$dlta13CCo2Refe$vari,
                         d13C_ref_n=med$dlta13CCo2Refe$numSamp,d13C_ref_btime=med$dlta13CCo2Refe$timeBgn,
                         d13C_ref_etime=med$dlta13CCo2Refe$timeEnd,CO2_ref_conc=med$rtioMoleDryCo2Refe$mean,
                         conc12CCO2_obs=med$rtioMoleDry12CCo2$mean,conc13CCO2_obs=med$rtioMoleDry13CCo2$mean)
    
    # calculate 12CO2 and 13CO2 concentrations for medium standard
    # for reference and observed isotope ratios
    med_rs <- med_rs %>%
      mutate(std_name="med") %>%
      mutate(conc12CCO2_ref = CO2_ref_conc*(1-0.00474)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
      mutate(conc13CCO2_ref = CO2_ref_conc*(1-0.00474)-conc12CCO2_ref) %>%
      #mutate(conc12CCO2_obs = CO2_meas_conc*(1-0.00474)/(1+R_vpdb*(1+d13C_meas_mean/1000))) %>%
      #mutate(conc13CCO2_obs = CO2_meas_conc*(1-0.00474)-conc12CCO2_obs) %>%
      mutate(d13C_meas_btime=as.POSIXct(d13C_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")) # for assigning times later.
    
    
    # "low" standard
    low_rs <- data.frame(d13C_meas_mean=low$dlta13CCo2$mean,d13C_meas_var=low$dlta13CCo2$vari,
                         CO2_meas_conc=low$rtioMoleDryCo2$mean,d13C_meas_n=low$dlta13CCo2$numSamp,
                         d13C_meas_btime=low$dlta13CCo2$timeBgn,d13C_meas_etime=low$dlta13CCo2$timeEnd,
                         d13C_ref_mean=low$dlta13CCo2Refe$mean,d13C_ref_var=low$dlta13CCo2Refe$vari,
                         d13C_ref_n=low$dlta13CCo2Refe$numSamp,d13C_ref_btime=low$dlta13CCo2Refe$timeBgn,
                         d13C_ref_etime=low$dlta13CCo2Refe$timeEnd,CO2_ref_conc=low$rtioMoleDryCo2Refe$mean,
                         conc12CCO2_obs=low$rtioMoleDry12CCo2$mean,conc13CCO2_obs=low$rtioMoleDry13CCo2$mean)
    
    # calculate 12CO2 and 13CO2 concentrations for low standard
    # for reference and observed isotope ratios
    low_rs <- low_rs %>%
      mutate(std_name="low") %>%
      mutate(conc12CCO2_ref = CO2_ref_conc*(1-0.00474)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
      mutate(conc13CCO2_ref = CO2_ref_conc*(1-0.00474)-conc12CCO2_ref) %>%
      #mutate(conc12CCO2_obs = CO2_meas_conc*(1-0.00474)/(1+R_vpdb*(1+d13C_meas_mean/1000))) %>%
      #mutate(conc13CCO2_obs = CO2_meas_conc*(1-0.00474)-conc12CCO2_obs) %>%
      mutate(d13C_meas_btime=as.POSIXct(d13C_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")) # for assigning times later.
    
    # need to add propogation of uncertainty sometime! - RPF
    
    # check to make sure there are the same number of observations for each standard.
    print(c(nrow(high_rs),nrow(med_rs),nrow(low_rs)))
    
    if (!(identical(nrow(high_rs),nrow(med_rs)) & identical(nrow(high_rs),nrow(low_rs)))) {
      # if above logical evaluates as true, this means that the standards 
      # have a different number of observations.
      
      print(!(identical(nrow(high_rs),nrow(med_rs)) & identical(nrow(high_rs),nrow(low_rs))))
      
      high_rs <- high_rs %>%
        mutate(dom = day(d13C_meas_btime)) %>% # get day of month
        group_by(dom) %>%
        slice(1) %>%
        ungroup()
      
      med_rs <- med_rs %>%
        mutate(dom = day(d13C_meas_btime)) %>% # get day of month
        group_by(dom) %>%
        slice(1) %>%
        ungroup()
      
      low_rs <- low_rs %>%
        mutate(dom = day(d13C_meas_btime)) %>% # get day of month
        group_by(dom) %>%
        slice(1) %>%
        ungroup()
      
      # Target for improvement: might be a less "blind" way of selecting one observation per day.
    }
    
    #-------------------------------------------------------------------------------------
    # calculate gain and offset values (eq. 2 and 3 of Bowling et al. 2003)
    # use high and low values, and validate w/ medium standards.
    
    gain12C <- (high_rs$conc12CCO2_ref - low_rs$conc12CCO2_ref)/(high_rs$conc12CCO2_obs - low_rs$conc12CCO2_obs)
    gain13C <- (high_rs$conc13CCO2_ref - low_rs$conc13CCO2_ref)/(high_rs$conc13CCO2_obs - low_rs$conc13CCO2_obs)
    
    offset12C <- high_rs$conc12CCO2_ref - gain12C*high_rs$conc12CCO2_obs
    offset13C <- high_rs$conc13CCO2_ref - gain13C*high_rs$conc13CCO2_obs
    
    #-----------------------------------------------------------------
    # perform validation
    
    est.med.12C <- med_rs$conc12CCO2_obs*gain12C + offset12C
    est.med.13C <- med_rs$conc13CCO2_obs*gain13C + offset13C
    
    diff.12C <- est.med.12C - med_rs$conc12CCO2_ref
    diff.13C <- est.med.13C - med_rs$conc13CCO2_ref
    diff.delta <- 1000*(est.med.13C/est.med.12C/R_vpdb - 1) - 1000*(med_rs$conc13CCO2_ref/med_rs$conc12CCO2_ref/R_vpdb-1)
    
    RPF.calVal.flag <- ifelse(abs(diff.delta) < 0.5, # weak constraint...ppm values look quite good, but if 0.1 always fails...
                              1, # set to 1 if passes calibration validation
                              0) # set to 0 if fails calibratino validation
    
    #--------------------------------------------------------------------
    # create output data frame...
    #--------------------------------------------------------------------
    # get start and end times from high standard. apply each calibration
    # forward in time to the next calibration point.
    # loop through times, assign beginning, ending value. max etime should be just fine.
    starttimes <- vector()
    endtimes <- vector()
    
    # specify beignning,end of calibratino periods.
    for (i in 1:nrow(high_rs)) {
      starttimes[i] <- ifelse(i !=1, 
                              high_rs$d13C_meas_btime[i],
                              floor_date(high_rs$d13C_meas_btime[i],unit="month")) # round to beginning of month if at the first row
      endtimes[i] <- ifelse(i != nrow(high_rs), 
                            high_rs$d13C_meas_btime[i+1], 
                            ceiling_date(high_rs$d13C_meas_btime[i],unit="month")) # round to end of month if at last row
    }
    
    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start=as.POSIXct(starttimes,tz="UTC",origin="1970-01-01"),
                      end=as.POSIXct(endtimes,tz="UTC",origin="1970-01-01"),
                      gain12C,gain13C,offset12C,offset13C,
                      diff.12C,diff.13C,diff.delta,
                      RPF.calVal.flag)
    
    var_for_h5 <- out
    
    var_for_h5$start <- as.character(paste0(year(out$start),"-",
                                            ifelse(month(out$start)<10,paste0("0",month(out$start)),month(out$start)),"-",
                                            ifelse(day(out$start)<10,paste0("0",day(out$start)),day(out$start)),"T",
                                            ifelse(hour(out$start)<10,paste0("0",hour(out$start)),hour(out$start)),":",
                                            ifelse(minute(out$start)<10,paste0("0",minute(out$start)),minute(out$start)),":",
                                            ifelse(second(out$start)<10,paste0("0",second(out$start)),second(out$start)),"Z"))
    
    var_for_h5$end <- as.character(paste0(year(out$end),"-",
                                          ifelse(month(out$end)<10,paste0("0",month(out$end)),month(out$end)),"-",
                                          ifelse(day(out$end)<10,paste0("0",day(out$end)),day(out$end)),"T",
                                          ifelse(hour(out$end)<10,paste0("0",hour(out$end)),hour(out$end)),":",
                                          ifelse(minute(out$end)<10,paste0("0",minute(out$end)),minute(out$end)),":",
                                          ifelse(second(out$end)<10,paste0("0",second(out$end)),second(out$end)),"Z"))
    
    var_for_h5$valid_period_start <- var_for_h5$start
    var_for_h5$valid_period_end   <- var_for_h5$end
    
    # remove old vars.
    var_for_h5$start <- var_for_h5$end <- NULL
    
    # okay try to write out to h5 file.
    fid <- H5Fopen(fname)
    
    co2.cal.outloc <- H5Gopen(fid,paste0('/',site,'/dp01iso/data/isoCo2'))
    
    # write out dataset.
    h5writeDataset.data.frame(obj = var_for_h5,h5loc=co2.cal.outloc,name="calGainsOffsets",DataFrameAsCompound = TRUE)
    
    # close the group and the file
    H5Gclose(co2.cal.outloc)
    H5Fclose(fid)
    
    # return the var.
    return(out) # out has time variables as POSIXct, not in the same format as NEON data files.
    
    #-----------------------------------------------------------------
    # 
    #--------------------------------------------------------------------------------------- 
    #======================================================================================= 
  } else if (method == 2) {
    
    # attempt to pull relevent data out to a single dataframe.
    high_rs <- data.frame(d13C_meas_mean=high$dlta13CCo2$mean,
                          d13C_meas_var=high$dlta13CCo2$vari,d13C_meas_n=high$dlta13CCo2$numSamp,
                          d13C_meas_btime=high$dlta13CCo2$timeBgn,d13C_meas_etime=high$dlta13CCo2$timeEnd,
                          d13C_ref_mean=high$dlta13CCo2Refe$mean,d13C_ref_var=high$dlta13CCo2Refe$vari,
                          d13C_ref_n=high$dlta13CCo2Refe$numSamp,d13C_ref_btime=high$dlta13CCo2Refe$timeBgn,
                          d13C_ref_etime=high$dlta13CCo2Refe$timeEnd)
    
    high_rs <- high_rs %>%
      mutate(std_name="high")
    
    med_rs <- data.frame(d13C_meas_mean=med$dlta13CCo2$mean,
                         d13C_meas_var=med$dlta13CCo2$vari,d13C_meas_n=med$dlta13CCo2$numSamp,
                         d13C_meas_btime=med$dlta13CCo2$timeBgn,d13C_meas_etime=med$dlta13CCo2$timeEnd,
                         d13C_ref_mean=med$dlta13CCo2Refe$mean,d13C_ref_var=med$dlta13CCo2Refe$vari,
                         d13C_ref_n=med$dlta13CCo2Refe$numSamp,d13C_ref_btime=med$dlta13CCo2Refe$timeBgn,
                         d13C_ref_etime=med$dlta13CCo2Refe$timeEnd)
    
    med_rs <- med_rs %>%
      mutate(std_name="med")
    
    low_rs <- data.frame(d13C_meas_mean=low$dlta13CCo2$mean,
                         d13C_meas_var=low$dlta13CCo2$vari,d13C_meas_n=low$dlta13CCo2$numSamp,
                         d13C_meas_btime=low$dlta13CCo2$timeBgn,d13C_meas_etime=low$dlta13CCo2$timeEnd,
                         d13C_ref_mean=low$dlta13CCo2Refe$mean,d13C_ref_var=low$dlta13CCo2Refe$vari,
                         d13C_ref_n=low$dlta13CCo2Refe$numSamp,d13C_ref_btime=low$dlta13CCo2Refe$timeBgn,
                         d13C_ref_etime=low$dlta13CCo2Refe$timeEnd)
    
    low_rs <- low_rs %>%
      mutate(std_name="low")
    
    #=======================================================================
    # apply calibration routines
    #======================================================================= 
    # bind together, and cleanup.
    stds <- do.call(rbind,list(high_rs,med_rs,low_rs))
    rm(high_rs,med_rs,low_rs,high,med,low)
    
    # replace NaNs with NA
    # rpf note on 181121 - what does this line actually do? Seems tautological.
    # rpf note 181126 - is.na() also returns NaN as NA, so this does actually do what first
    # comment indicates.
    stds[ is.na(stds) ] <- NA
    
    # change class of time variables from charatcter to posixct.
    stds$d13C_meas_btime <- as.POSIXct(stds$d13C_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    stds$d13C_meas_etime <- as.POSIXct(stds$d13C_meas_etime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    
    stds$d13C_ref_btime <- as.POSIXct(stds$d13C_ref_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    stds$d13C_ref_etime <- as.POSIXct(stds$d13C_ref_etime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    
    # reorder data frame
    stds <- stds[order(stds$d13C_meas_btime),]
    
    # assign a vector corresponding to calibration period.
    stds$cal_period <- stds$d13C_meas_n
    
    period_id <- 1
    tdiffs <- c(diff(stds$d13C_meas_btime),0)
    for (i in 1:nrow(stds)) {
      stds$cal_period[i] <- period_id   
      if (tdiffs[i] >= time.diff.betweeen.standards) {period_id = period_id + 1}
    }
    
    # okay, now run calibrations...
    #------------------------------
    # create output variables.
    cal_slopes <- vector()
    cal_ints   <- vector()
    cal_rsq    <- vector()
    
    for (i in 2:max(stds$cal_period)) {
      # subset data.
      cal.subset <- stds[which(stds$cal_period==i | stds$cal_period==(i-1)),]
      
      if (!all(is.na(cal.subset$d13C_meas_mean)) & !all(is.na(cal.subset$d13C_ref_mean))) { # ensure that not all cases are missing.
        tmp <- lm(d13C_ref_mean ~ d13C_meas_mean,data=cal.subset)
        
        cal_slopes[i-1] <- coef(tmp)[[2]]
        cal_ints[i-1] <- coef(tmp)[[1]]
        cal_rsq[i-1] <- summary(tmp)$r.squared
      } else {
        cal_slopes[i-1] <- NA
        cal_ints[i-1] <- NA
        cal_rsq[i-1] <- NA
      }
    }
    
    # make dataframe of calibration data.
    times <- stds %>%
      select(d13C_meas_btime,d13C_meas_etime,d13C_ref_btime,d13C_ref_etime,cal_period) %>%
      group_by(cal_period) %>%
      summarize(etime = max(c(d13C_meas_etime,d13C_ref_etime)))
    
    # loop through times, assign beginning, ending value. max etime should be just fine.
    starttimes <- vector()
    endtimes <- vector()
    
    for (i in 1:length(cal_slopes)) {
      starttimes[i] <- times$etime[i]
      endtimes[i] <- times$etime[i+1]
    }
    
    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start=as.POSIXct(starttimes,tz="UTC",origin="1970-01-01"),
                      end=as.POSIXct(endtimes,tz="UTC",origin="1970-01-01"),
                      slope=cal_slopes,intercept=cal_ints,r2=cal_rsq)
    
    var_for_h5 <- out
    
    var_for_h5$start <- as.character(paste0(year(out$start),"-",
                                            ifelse(month(out$start)<10,paste0("0",month(out$start)),month(out$start)),"-",
                                            ifelse(day(out$start)<10,paste0("0",day(out$start)),day(out$start)),"T",
                                            ifelse(hour(out$start)<10,paste0("0",hour(out$start)),hour(out$start)),":",
                                            ifelse(minute(out$start)<10,paste0("0",minute(out$start)),minute(out$start)),":",
                                            ifelse(second(out$start)<10,paste0("0",second(out$start)),second(out$start)),"Z"))
    
    var_for_h5$end <- as.character(paste0(year(out$end),"-",
                                          ifelse(month(out$end)<10,paste0("0",month(out$end)),month(out$end)),"-",
                                          ifelse(day(out$end)<10,paste0("0",day(out$end)),day(out$end)),"T",
                                          ifelse(hour(out$end)<10,paste0("0",hour(out$end)),hour(out$end)),":",
                                          ifelse(minute(out$end)<10,paste0("0",minute(out$end)),minute(out$end)),":",
                                          ifelse(second(out$end)<10,paste0("0",second(out$end)),second(out$end)),"Z"))
    
    var_for_h5$valid_period_start <- var_for_h5$start
    var_for_h5$valid_period_end   <- var_for_h5$end
    
    # remove old vars.
    var_for_h5$start <- var_for_h5$end <- NULL
    
    # okay try to write out to h5 file.
    fid <- H5Fopen(fname)
    
    co2.cal.outloc <- H5Gopen(fid,paste0('/',site,'/dp01iso/data/isoCo2'))
    
    # write out dataset.
    h5writeDataset.data.frame(obj = var_for_h5,h5loc=co2.cal.outloc,name="calRegressions",DataFrameAsCompound = TRUE)
    
    # close the group and the file
    H5Gclose(co2.cal.outloc)
    H5Fclose(fid)
    
    # return the var.
    return(out) # out has time variables as POSIXct, not in the same format as NEON data files.
  } else {
    print("This code should never be run - shouldn't be possible given stop statement above.")
  }
} # end function.