#' calibrate_carbon_Bowling2003
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param inname Name of the input file.
#' @param outname Name of the output file.
#' @param time.diff.between.standards Time (in seconds) required between consecutive standard measurements.
#' @param force.cal.to.beginning Extend first calibration to the beginning of the file? (Default true)
#' @param force.cal.to.end Extend last calibration to the end of the file? (Detault true)
#' @param site Four letter NEON site code for site being processed.
#'
#' @return Returns nothing to the workspace, but creates a new output file.
#' @export
#'
#' 
calibrate_carbon_Bowling2003 <- function(inname,outname,site,time.diff.between.standards=1800,
                                         force.cal.to.beginning=TRUE,force.cal.to.end=TRUE) {

  #------------------------------------------------------------
  # Print some information before starting data processing
  #------------------------------------------------------------
  print("Processing carbon calibration data...")
  
  print("Applying method 1: two-point mixing ratio gain and offset calibration")
  print("Uses third point as calibration verification.")
  print("Reference: Bowling et al. 2003 AFM")
  
  #-----------------------------------------------------------
  # specify a few parameters for the Bowling method.
  
  f <- 0.00474  # fraction of CO2 isotopomers that aren't 12CO2 or 13CO2
                # note: f technically varies, but this has little impact
                # on calibration per Griffis et al. 2004.
  
  R_vpdb <- 0.0111797 # 13C/12C ratio for VPD standard.
  
  #-----------------------------------------------------------
  # pull all carbon isotope data into a list.
  ciso <- h5read(inname,paste0('/',site,'/dp01/data/isoCo2'))
  
  # extract standards data.
  high <- ciso$co2High_09m
  med <- ciso$co2Med_09m
  low <- ciso$co2Low_09m

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
    mutate(conc12CCO2_ref = CO2_ref_conc*(1-f)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
    mutate(conc13CCO2_ref = CO2_ref_conc*(1-f)-conc12CCO2_ref) %>%
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
    mutate(conc12CCO2_ref = CO2_ref_conc*(1-f)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
    mutate(conc13CCO2_ref = CO2_ref_conc*(1-f)-conc12CCO2_ref) %>%
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
    mutate(conc12CCO2_ref = CO2_ref_conc*(1-f)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
    mutate(conc13CCO2_ref = CO2_ref_conc*(1-f)-conc12CCO2_ref) %>%
    mutate(d13C_meas_btime=as.POSIXct(d13C_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")) # for assigning times later.
  
  # need to add propogation of uncertainty sometime! - RPF
  
  #--------------------------------------------------------------
  # Ensure there are the same number of standard measurements for each standard.
  #--------------------------------------------------------------
  
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
  # try to determine if all data points are valid. most obvious check here that 
  # should remove the most heinous values: are measured [CO2] w/in some tolerance
  # of expected [CO2]? This will help scrub out bad data from empty tanks, etc.
  
  conc_thres <- 25 # threshold in ppm.
  
  # need to make a list of how many good calibration points there are for each calibration period.
  val.df <- data.frame(low=ifelse(abs(low_rs$CO2_meas_conc - low_rs$CO2_ref_conc) < conc_thres,
                                  1,0), # 1 if true, 0 if false
                       med=ifelse(abs(med_rs$CO2_meas_conc - med_rs$CO2_ref_conc) < conc_thres,
                                  1,0),
                       high=ifelse(abs(high_rs$CO2_meas_conc - high_rs$CO2_ref_conc) < conc_thres,
                                   1,0))
  
  # add row sum.
  val.df$tot <- rowSums(val.df,na.rm=TRUE) # make sure to remove NAs
  
  print(val.df)
  
  # there's almost definitely a faster way to implement this, but coding as a loop for now.
  #-----------------------------------------------------------------------
  # preallocate variables.
  gain12C <- gain13C <- vector(length = nrow(high_rs))
  offset12C <- offset13C <- vector(length = nrow(high_rs))
  
  for (i in 1:nrow(val.df)) {
    if (!is.na(val.df$tot[i]) & val.df$tot[i] == 3) { # e.g., all calibration points are good.
      # all points are good, so calibrate gain and offset w/ high and low points.
      gain12C[i] <- (high_rs$conc12CCO2_ref[i] - low_rs$conc12CCO2_ref[i])/(high_rs$conc12CCO2_obs[i] - low_rs$conc12CCO2_obs[i])
      gain13C[i] <- (high_rs$conc13CCO2_ref[i] - low_rs$conc13CCO2_ref[i])/(high_rs$conc13CCO2_obs[i] - low_rs$conc13CCO2_obs[i])
      
      offset12C[i] <- high_rs$conc12CCO2_ref[i] - gain12C[i]*high_rs$conc12CCO2_obs[i]
      offset13C[i] <- high_rs$conc13CCO2_ref[i] - gain13C[i]*high_rs$conc13CCO2_obs[i]
    } else if (!is.na(val.df$tot[i]) & val.df$tot[i] == 2) { # 1 calibration point doesn't pass test(s)
      # need to determine which two points are good, which can be done w/ 2 logical tests.
      
      if (!is.na(val.df$tot[i]) & !is.na(val.df$low[i]) & val.df$low[i] == 1) { # low point is good, need to determine if med or high point is
                                # other valid point.
        if (!is.na(val.df$tot[i]) & !is.na(val.df$med[i]) & val.df$med[i] == 1) { # low and medium point are valid.
          
          gain12C[i] <- (med_rs$conc12CCO2_ref[i] - low_rs$conc12CCO2_ref[i])/(med_rs$conc12CCO2_obs[i] - low_rs$conc12CCO2_obs[i])
          gain13C[i] <- (med_rs$conc13CCO2_ref[i] - low_rs$conc13CCO2_ref[i])/(med_rs$conc13CCO2_obs[i] - low_rs$conc13CCO2_obs[i])
          
          offset12C[i] <- med_rs$conc12CCO2_ref[i] - gain12C[i]*med_rs$conc12CCO2_obs[i]
          offset13C[i] <- med_rs$conc13CCO2_ref[i] - gain13C[i]*med_rs$conc13CCO2_obs[i] 
        
        } else { # low and high only are good.
        
          gain12C[i] <- (high_rs$conc12CCO2_ref[i] - low_rs$conc12CCO2_ref[i])/(high_rs$conc12CCO2_obs[i] - low_rs$conc12CCO2_obs[i])
          gain13C[i] <- (high_rs$conc13CCO2_ref[i] - low_rs$conc13CCO2_ref[i])/(high_rs$conc13CCO2_obs[i] - low_rs$conc13CCO2_obs[i])
          
          offset12C[i] <- high_rs$conc12CCO2_ref[i] - gain12C[i]*high_rs$conc12CCO2_obs[i]
          offset13C[i] <- high_rs$conc13CCO2_ref[i] - gain13C[i]*high_rs$conc13CCO2_obs[i]
        }
      } else { # MUST be medium and high points that are good.
        
        gain12C[i] <- (high_rs$conc12CCO2_ref[i] - med_rs$conc12CCO2_ref[i])/(high_rs$conc12CCO2_obs[i] - med_rs$conc12CCO2_obs[i])
        gain13C[i] <- (high_rs$conc13CCO2_ref[i] - med_rs$conc13CCO2_ref[i])/(high_rs$conc13CCO2_obs[i] - med_rs$conc13CCO2_obs[i])
        
        offset12C[i] <- high_rs$conc12CCO2_ref[i] - gain12C[i]*high_rs$conc12CCO2_obs[i]
        offset13C[i] <- high_rs$conc13CCO2_ref[i] - gain13C[i]*high_rs$conc13CCO2_obs[i]  
      } # if low == 1
    } else if (is.na(val.df$tot[i]) | val.df$tot[i] < 2) {
      # can't really do anything here if less than 2 valid points, 
      # set as missing, and fill w/ last known good calibration later?
      
      gain12C[i] <- gain13C[i] <- offset12C[i] <- offset13C[i] <- NA
      
    }# if tot >= 2
  } # for

  #-----------------------------------------------------------------
  # perform validation
  
  est.med.12C <- med_rs$conc12CCO2_obs*gain12C + offset12C
  est.med.13C <- med_rs$conc13CCO2_obs*gain13C + offset13C
  
  diff.12C <- est.med.12C - med_rs$conc12CCO2_ref
  diff.13C <- est.med.13C - med_rs$conc13CCO2_ref
  diff.delta <- 1000*(est.med.13C/est.med.12C/R_vpdb - 1) - 1000*(med_rs$conc13CCO2_ref/med_rs$conc12CCO2_ref/R_vpdb-1)
  
  calVal.flag1 <- ifelse(abs(diff.delta) < 0.5, # weak constraint...ppm values look quite good, but if 0.1 always fails...
                            1, # set to 1 if passes calibration validation
                            0) # set to 0 if fails calibratino validation
  
  calVal.flag2 <- ifelse(val.df$tot > 1,
                         1, # set to pass if 2+ valid points.
                         0) # otherwise, set to fail.
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
                    calVal.flag1,calVal.flag2)
  
  var_for_h5 <- out
  
  var_for_h5$start <- convert_POSIXct_to_NEONhdf5_time(out$start)
  var_for_h5$end <- convert_POSIXct_to_NEONhdf5_time(out$end)
  
  var_for_h5$valid_period_start <- var_for_h5$start
  var_for_h5$valid_period_end   <- var_for_h5$end
  
  # remove old vars.
  var_for_h5$start <- var_for_h5$end <- NULL
  
  # okay try to write out to h5 file.
  h5createFile(outname)
  h5createGroup(outname,paste0('/',site))
  h5createGroup(outname,paste0('/',site,'/dp01iso'))
  h5createGroup(outname,paste0('/',site,'/dp01iso/data'))
  h5createGroup(outname,paste0('/',site,'/dp01iso/data/isoCo2'))
  
  fid <- H5Fopen(outname)

  # copy attributes from source file and write to output file.
  tmp <- h5readAttributes(inname,paste0('/',site))
  
  attrloc <- H5Gopen(fid,paste0('/',site))
  
  for (i in 1:length(tmp)) { # probably a more rapid way to do this in the future...lapply?
    h5writeAttribute(h5obj=attrloc,attr=tmp[[i]],name=names(tmp)[i])
  }
  
  H5Gclose(attrloc)
  
  co2.cal.outloc <- H5Gopen(fid,paste0('/',site,'/dp01iso/data/isoCo2'))
  
  # write out dataset.
  h5writeDataset.data.frame(obj = var_for_h5,h5loc=co2.cal.outloc,
                            name="calGainsOffsets",
                            DataFrameAsCompound = TRUE)
  
  # close the group and the file
  H5Gclose(co2.cal.outloc)
  H5Fclose(fid)
  h5closeAll()  
  
  #----------------------------------------------------------------------------------------
  # calibrate ambient data.

  # calibrate data for each height.
  #-------------------------------------
  # extract ambient measurements from ciso
  ciso_logical <- grepl(pattern="000",x=names(ciso))
  ciso_subset <- ciso[ciso_logical]

  lapply(names(ciso_subset),
         function(x){calibrate_ambient_carbon_Bowling2003(amb.data.list=ciso_subset[[x]],
                                        caldf=out,outname=x,file=outname,site=site)})
  
  h5closeAll()
}
