#' calibrate_carbon_Bowling2003
#' 
#' Use the gain-and-offset style calibration approach detailed in Bowling et al. 2003 AFM.
#' Wen et al. 2011 compared several different carbon isotope calibration techniques and
#' found this to be the superior method under most circumstances. In brief, this method takes
#' two of the measurement and reference CO2 and d13C values for two of the three daily reference
#' materials to define a correction equation that defines gain and offset parameters to correct
#' the observations to the VPDB scale. These gain and offset parameters are analagous to 
#' regression slope and intercepts, but jointly correct for CO2 concentration dependence and place
#' d13C values on the VPDB scale. Gain and offset parameters are determined independently for
#' each major isotopologue (12CO2 and 13CO2). For the two reference materials selected, the gain and offset
#' parameters are defined by: 
#' \deqn{G = (X_{2,ref}-X_{1,ref})/(X_{2,meas}-X_{1,meas})}
#' \deqn{O = X_{2,ref}- G X_{2,meas}}
#' Calibrated ambient isotopologues are then given using these gain and offset parameters as:
#' \deqn{X_{cal} = X_{meas} G + O}
#' 
#' Measurements of reference materials were considered "good" if the following conditions were met:
#' \itemize{
#'   \item Measured CO2 concentrations were within 10 ppm of known "reference" concentrations.
#'   \item Variance of the CO2 concentration in standard peak was less than 2 ppm.
#'   \item Measured d13C value must be within 3 per mil of known "reference" d13C value.
#' }
#' The first two criteria are intended to filter out periods where there is a clear issue with the
#' gas delivery system (i.e., nearly empty gas tank, problem with a valve in the manifold, etc.); the third
#' criterion was adopted after visual inspection of data timeseries revealed that often the first
#' standard measurement following an instrument issue had a higher-than-expected error. This criterion
#' clips clearly poor values. 
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param inname Name of the input file. (character)
#' @param outname Name of the output file. (character)
#' @param time.diff.between.standards Time (in seconds) required between consecutive standard measurements. (numeric)
#' @param force.cal.to.beginning Extend first calibration to the beginning of the file? (CURRENTLY NOT USED)
#' @param force.cal.to.end Extend last calibration to the end of the file? (CURRENTLY NOT USED)
#' @param ucrt.source Where do we take uncertainty estimates from (data variance, or the ucrt group in the hdf5 file?)
#' @param site Four letter NEON site code for site being processed. (character)
#' @param interpolate.missing.cals If a calibration is flagged as bad, should we replace these calibrations by interpolating from the closest good calibrations? (logical)
#' @param interpolation.method How to interpolate across bad calibrations, if \code{interpolate.missing.cals == TRUE}. (character)
#' @param filter.ambient Apply the median absolute deviation filter (Brock 86) to remove impulse spikes in output ambient data? (logical; default true)
#'
#' @return Returns nothing to the workspace, but creates a new output HDF5 file containing calibrated carbon isotope values.
#' @export
#'
#' @importFrom magrittr %>%
#' 
calibrate_carbon_Bowling2003 <- function(inname,
                                         outname,
                                         site,
                                         time.diff.between.standards=1800,
                                         force.cal.to.beginning=TRUE,
                                         force.cal.to.end=TRUE,
                                         interpolate.missing.cals=TRUE,
                                         interpolation.method="LWMA", 
                                         ucrt.source="data",
                                         filter.ambient=TRUE) {
  #------------------------------------------------------------
  # Print some information before starting data processing
  #------------------------------------------------------------
  print("Processing carbon calibration data...")
  print("Applying method 1: two-point mixing ratio gain and offset calibration")
  print("Uses third point as calibration verification.")
  print("Reference: Bowling et al. 2003 AFM")
  #----------------------------------------------------------
  # specify a few parameters for the Bowling method.
  
  f <- 0.00474  # fraction of CO2 isotopomers that aren't 12CO2 or 13CO2
                # note: f technically varies, but this has little impact
                # on calibration per Griffis et al. 2004.
  
  R_vpdb <- 0.0111797 # 13C/12C ratio for VPD standard.
  
  #-----------------------------------------------------------
  # pull all carbon isotope data into a list.
  
  ciso <- rhdf5::h5read(inname,paste0('/',site,'/dp01/data/isoCo2'))
  ucrt <- rhdf5::h5read(inname,paste0('/',site,'/dp01/ucrt/isoCo2'))
  
  high_rs <- extract_carbon_calibration_data(ciso,ucrt,"high")
  med_rs  <- extract_carbon_calibration_data(ciso,ucrt,"med")
  low_rs  <- extract_carbon_calibration_data(ciso,ucrt,"low")
    
  # combine data frames, calculate derived variables, and then separate back out.
  standards <- do.call(rbind,list(low_rs,med_rs,high_rs))
  rm(high_rs,med_rs,low_rs)
  
  standards <- standards %>%
    dplyr::mutate(d13C_obs_btime=as.POSIXct(d13C_obs_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")) %>% # for assigning times later. 
    #------------------------------------------------------------
    # calculate mole fraction of 12CO2 and 13CO2 for the reference gases and observed values.
    dplyr::mutate(conc12CCO2_ref = CO2_ref_mean*(1-f)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
    dplyr::mutate(conc13CCO2_ref = CO2_ref_mean*(1-f)-conc12CCO2_ref) %>%
    dplyr::mutate(conc12CCO2_obs = CO2_obs_mean*(1-f)/(1+R_vpdb*(1+d13C_obs_mean/1000))) %>%
    dplyr::mutate(conc13CCO2_obs = CO2_obs_mean*(1-f)-conc12CCO2_obs) 
    
  # split back out into 3 data frames for each standard.
  low_rs <- dplyr::filter(standards,std_name=="low")
  med_rs <- dplyr::filter(standards,std_name=="med")
  high_rs <- dplyr::filter(standards,std_name=="high")
    
  rm(standards)
  
  #--------------------------------------------------------------
  # Ensure there are the same number of standard measurements for each standard.
  #--------------------------------------------------------------
    # 191024 rpf - prior versions of this have just sliced out the first observation per day.
    # however, the most common cause of multiple standards to be analyzed per day is a 
    # malfunctioning valve in the manifold that causes the same standard gas to register as multiple
    # peaks. each peak is shorter, higher variance, and doesn't allow even the CO2 concentration
    # to stabilize. until further notice, i suggest removing these standards altogether.
    # code below has been modified to achieve this.
    
    high_rs <- high_rs %>%
      dplyr::mutate(dom = day(d13C_obs_btime)) %>% # get day of month
      dplyr::group_by(dom) %>%
      dplyr::filter(d13C_obs_n > 200 | is.na(d13C_obs_n)) %>% # check to make sure peak sufficiently long, then slice off single.
      dplyr::slice(1) %>%
      dplyr::ungroup()
    
    med_rs <- med_rs %>%
      dplyr::mutate(dom = day(d13C_obs_btime)) %>% # get day of month
      dplyr::group_by(dom) %>%
      dplyr::filter(d13C_obs_n > 200 | is.na(d13C_obs_n)) %>% # check to make sure peak sufficiently long, then slice off single.
      dplyr::slice(1) %>%
      dplyr::ungroup()
    
    low_rs <- low_rs %>%
      dplyr::mutate(dom = day(d13C_obs_btime)) %>% # get day of month
      dplyr::group_by(dom) %>%
      dplyr::filter(d13C_obs_n > 200 | is.na(d13C_obs_n)) %>% # check to make sure peak sufficiently long, then slice off single.
      dplyr::slice(1) %>%
      dplyr::ungroup()
  
  # filter to only common days?
  common_days <- Reduce(intersect,list(low_rs$dom,med_rs$dom,high_rs$dom))
  
  low_rs <- low_rs %>%
    dplyr::filter(dom %in% common_days)
  med_rs <- med_rs %>%
    dplyr::filter(dom %in% common_days)
  high_rs <- high_rs %>%
    dplyr::filter(dom %in% common_days)
  
  #------------------------------------------------------------
  # OLD CODE - LEFT HERE FOR REFERENCE
  # if (!(identical(nrow(high_rs),nrow(med_rs)) & identical(nrow(high_rs),nrow(low_rs)))) {
  #   # if above logical evaluates as true, this means that the standards 
  #   # have a different number of observations.
  #   high_rs <- high_rs %>%
  #     mutate(dom = day(d13C_obs_btime)) %>% # get day of month
  #     group_by(dom) %>%
  #     slice(1) %>% # require group count to be 1 per note above.
  #     ungroup()
  #-------------------------------------------------------------------------------------
  # try to determine if all data points are valid. most obvious check here that 
  # should remove the most heinous values: are measured [CO2] w/in some tolerance
  # of expected [CO2]? This will help scrub out bad data from empty tanks, etc.
  
  conc_thres <- 15 # threshold in ppm.
  conc_var_thres <- 5 # threshold for co2 variance in ppm.
  d13C_diff_thres <- 3 # absolute deviation of d13C value allowed for standards. 3 per mil chosen based on visual inspection of all data.
  
  # need to make a list of how many good calibration points there are for each calibration period.
  val.df <- data.frame(low=ifelse(abs(low_rs$CO2_obs_mean - low_rs$CO2_ref_mean) < conc_thres &
                                 low_rs$CO2_obs_var < conc_var_thres &
                                 abs(low_rs$d13C_obs_mean - low_rs$d13C_ref_mean) < d13C_diff_thres &  
                                 !is.na(low_rs$d13C_obs_mean) & !is.na(low_rs$d13C_ref_mean),
                                  1,0), # 1 if true, 0 if false
                       med=ifelse(abs(med_rs$CO2_obs_mean - med_rs$CO2_ref_mean) < conc_thres &
                                    med_rs$CO2_obs_var < conc_var_thres &
                                    abs(med_rs$d13C_obs_mean - med_rs$d13C_ref_mean) < d13C_diff_thres &  
                                    !is.na(med_rs$d13C_obs_mean) & !is.na(med_rs$d13C_ref_mean),
                                  1,0),
                       high=ifelse(abs(high_rs$CO2_obs_mean - high_rs$CO2_ref_mean) < conc_thres &
                                     high_rs$CO2_obs_var < conc_var_thres &
                                     abs(high_rs$d13C_obs_mean - high_rs$d13C_ref_mean) < d13C_diff_thres &  
                                     !is.na(high_rs$d13C_obs_mean) & !is.na(high_rs$d13C_ref_mean),
                                  1,0))
  
  # add row sum.
  if (nrow(val.df) == 0) {
    val.df <- data.frame(low=NA,med=NA,high=NA)
  }
  
  val.df$tot <- rowSums(val.df,na.rm=TRUE) # make sure to remove NAs
  #print(val.df$tot)
  
  # -----------------------------------------------------------------------
  # step above determined how many calibration points were good, next step is 
  # to determine *which* of these calibration points pass the test.
  #
  # there's almost definitely a faster way to implement this, but coding as a loop for now.
  #-----------------------------------------------------------------------
  # preallocate variables.
  cal.vals <- list()
  
  for (i in 1:nrow(val.df)) {
    
    if (!is.na(val.df$tot[i]) & val.df$tot[i] == 3) { # e.g., all calibration points are good.

      # if all points are good, use the high and low standards
      cal.vals[[i]] <- calculate_gain_and_offset(high_rs[i,],low_rs[i,])
    
    } else if (!is.na(val.df$tot[i]) & val.df$tot[i] == 2) { # 1 calibration point doesn't pass test(s)
      
      # need to determine which two points are good, which can be done w/ 2 logical tests.
      if (!is.na(val.df$tot[i]) & !is.na(val.df$low[i]) & val.df$low[i] == 1) { # low point is good, need to determine if med or high point is
                                                                                # other valid point.
      
        if (!is.na(val.df$tot[i]) & !is.na(val.df$med[i]) & val.df$med[i] == 1) { # low and medium point are valid.
        
            # medium and low standards pass the tests, so calculate gain and offset on these.
            cal.vals[[i]] <- calculate_gain_and_offset(med_rs[i,],low_rs[i,])
            
        } else { # low and high only are good.
          
          # medium standard is off, so use low and high standards.
          cal.vals[[i]] <- calculate_gain_and_offset(high_rs[i,],low_rs[i,])
          
        }
        
      } else { # MUST be medium and high points that are good.
        
        cal.vals[[i]] <- calculate_gain_and_offset(high_rs[i,],med_rs[i,])
        
      } # end if low == 1
      
    } else if (is.na(val.df$tot[i]) | val.df$tot[i] < 2) {
      
      # can't really do anything here if less than 2 valid points, 
      # set as missing, and fill w/ last known good calibration later?
      cal.vals[[i]] <- data.frame("gain12C"=NA,"gain13C"=NA,"offset12C"=NA,"offset13C"=NA)
      
    }# if tot >= 2
  } # for loop

  cal.vals <- do.call(rbind,cal.vals)
  names(cal.vals) <- c("gain12C","gain13C","offset12C","offset13C")

  #-----------------------------------------------------------------
  # perform validation
  est.med.12C <- med_rs$conc12CCO2_obs*cal.vals$gain12C + cal.vals$offset12C
  est.med.13C <- med_rs$conc13CCO2_obs*cal.vals$gain13C + cal.vals$offset13C
  
  calDelUcrt <- vector()
  calCO2Ucrt <- vector()
  
  for (i in 1:nrow(val.df)) {
    calDelUcrt[i] <- ifelse(val.df$tot[i] == 3,
                         sqrt((1000*(est.med.13C[i]/est.med.12C[i]/R_vpdb - 1) - med_rs$d13C_ref_mean[i])^2),
                         NA)
    
    calCO2Ucrt[i] <- ifelse(val.df$tot[i] == 3,
                            sqrt(((est.med.12C + est.med.13C)/(1-f) - med_rs$CO2_ref_mean[i])^2),
                            NA)
    
  }

  calgood <-  val.df$tot 
  
  #--------------------------------------------------------------------
  # perform interpolation, if requested.
  
  if (interpolate.missing.cals == TRUE) {
    
    if (sum(!is.na(cal.vals$gain12C)) > 5 & sum(val.df$tot) > 15) {

      # check to determine which method to use.
      if (interpolation.method == "LWMA") {
        # save a vector of which values have been replaced!
        replaced.vals <- ifelse(is.na(cal.vals$gain12C),1,0)
        
        print(paste0(100*sum(replaced.vals)/length(replaced.vals),"% of values filled w/ LWMA"))
        
        # linear weighted moving average chosen.
        cal.vals$gain12C <- imputeTS::na_ma(cal.vals$gain12C, weighting = "linear") 
        cal.vals$gain13C <- imputeTS::na_ma(cal.vals$gain13C, weighting = "linear")
        cal.vals$offset12C <- imputeTS::na_ma(cal.vals$offset12C, weighting = "linear") 
        cal.vals$offset13C <- imputeTS::na_ma(cal.vals$offset13C, weighting = "linear") 
        
      } else if (interpolation.method == "LOCF") {
        
        stop("LOCF not activated yet.")
      } else {
        stop("Interpolation method not recognized. Valid values currently are LOCF or LWMA, others to come if requested.")
      }
    } else {
      # set replaced.vals as 0, since none were replaced.
      replaced.vals <- rep(0,nrow(cal.vals))
    }
  }
  #--------------------------------------------------------------------
  # create output data frame...
  #--------------------------------------------------------------------
  # get start and end times from high standard. apply each calibration
  # forward in time to the next calibration point.
  # loop through times, assign beginning, ending value. max etime should be just fine.
  
  starttimes <- vector()
  endtimes <- vector()
  
  # specify beignning,end of calibration periods
  for (i in 1:nrow(high_rs)) {
    starttimes[i] <- ifelse(i !=1, 
                            high_rs$d13C_obs_btime[i],
                            floor_date(high_rs$d13C_obs_btime[i],unit="month")) # round to beginning of month if at the first row
    endtimes[i] <- ifelse(i != nrow(high_rs), 
                          high_rs$d13C_obs_btime[i+1], 
                          ceiling_date(high_rs$d13C_obs_btime[i],unit="month")) # round to end of month if at last row
  }
  
  # output dataframe giving valid time range, slopes, intercepts, rsquared.
  if (nrow(val.df) == 1 && is.na(val.df$low) && is.na(val.df$med) && is.na(val.df$high)) {
    if (any(is.na(starttimes)) | 
        any(is.na(endtimes))) {
      
      # break apart inname to get year/month combo.
      nm_tmp <- strsplit(inname,split=".",fixed=TRUE)
      yrmn <- nm_tmp[[1]][8]
      
      # should have length 7 - check here!
      if (!(nchar(yrmn) == 7)) {
        stop("Can't identify yr-month from file name!")
      }
      
      starttimes <- as.POSIXct(paste0(yrmn,"-15 00:00"),format="%Y-%m-%d %H:%M")      
      endtimes   <- as.POSIXct(paste0(yrmn,"-16 00:00"),format="%Y-%m-%d %H:%M")
      
    }
    
    out <- data.frame(start=as.POSIXct(starttimes,tz="UTC",origin="1970-01-01"),
                      end=as.POSIXct(starttimes,tz="UTC",origin="1970-01-01"),
                      calDelUcrt=NA,calCO2Ucrt=NA,calgood=NA,replaced.vals=NA)
  } else {
    out <- data.frame(start=as.POSIXct(starttimes,tz="UTC",origin="1970-01-01"),
                      end=as.POSIXct(endtimes,tz="UTC",origin="1970-01-01"),
                      calDelUcrt,calCO2Ucrt,calgood,replaced.vals)
  }

  out <- cbind(out,cal.vals)
  
  var_for_h5 <- out
  
  # out should now have 12 columns, check to see if this is true.
  if (!(ncol(var_for_h5) == 10)) {
    print(ncol(var_for_h5))
    stop("Output dataframe does not have proper row of columns - what happened?")
  }
  
  var_for_h5$start <- convert_POSIXct_to_NEONhdf5_time(out$start)
  var_for_h5$end <- convert_POSIXct_to_NEONhdf5_time(out$end)
  
  var_for_h5$valid_period_start <- var_for_h5$start
  var_for_h5$valid_period_end   <- var_for_h5$end
  
  # enforce that all other columns are numeric
  var_for_h5$gain12C <- as.numeric(var_for_h5$gain12C)
  var_for_h5$gain13C <- as.numeric(var_for_h5$gain13C)
  var_for_h5$offset12C <- as.numeric(var_for_h5$offset12C)
  var_for_h5$offset13C <- as.numeric(var_for_h5$offset13C)
  var_for_h5$calDelUcrt <- as.numeric(var_for_h5$calDelUcrt)
  var_for_h5$calCO2Ucrt <- as.numeric(var_for_h5$calCO2Ucrt)
  
  # remove old vars.
  var_for_h5$start <- var_for_h5$end <- NULL

  # okay try to write out to h5 file.
  rhdf5::h5createFile(outname)
  rhdf5::h5createGroup(outname,paste0('/',site))
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01'))
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/data'))
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/data/isoCo2'))
  
  fid <- rhdf5::H5Fopen(outname)
  
  # copy attributes from source file and write to output file.
  tmp <- rhdf5::h5readAttributes(inname,paste0('/',site))
  attrloc <- rhdf5::H5Gopen(fid,paste0('/',site))
  
  for (i in 1:length(tmp)) { # probably a more rapid way to do this in the future...lapply?
    rhdf5::h5writeAttribute(h5obj=attrloc,attr=tmp[[i]],name=names(tmp)[i])
  }
  
  rhdf5::H5Gclose(attrloc)
  
  # write out calibration dataframe to a new group to keep it away from stackEddy
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/data/isoCo2/calData'))
  co2.cal.outloc <- rhdf5::H5Gopen(fid,paste0('/',site,'/dp01/data/isoCo2/calData'))

  # write out dataset.
  rhdf5::h5writeDataset.data.frame(obj = var_for_h5,h5loc=co2.cal.outloc,
                            name="calGainsOffsets",
                            DataFrameAsCompound = TRUE)
  rhdf5::H5Gclose(co2.cal.outloc)

  #---------------------------------------------
  #---------------------------------------------
  # copy high/mid/low standard data from input file.
  #---------------------------------------------
  #---------------------------------------------
  #low
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/data/isoCo2/co2Low_09m'))
  low.outloc <- rhdf5::H5Gopen(fid,paste0('/',site,'/dp01/data/isoCo2/co2Low_09m'))

  low <- rhdf5::h5read(inname,paste0('/',site,'/dp01/data/isoCo2/co2Low_09m'))
  
  # kludge fix for now - need to add mean_cal column to low - but currently uncalibrated!
  low$dlta13CCo2$mean_cal <- low$dlta13CCo2$mean
  low$dlta13CCo2$mean_cal <- as.numeric(NA)
  
  low$rtioMoleDryCo2$mean_cal <- low$rtioMoleDryCo2$mean
  low$rtioMoleDryCo2$mean_cal <- as.numeric(NA)
  
  # loop through each of the variables in list amb.data.list and write out as a dataframe.
  lapply(names(low),function(x) {
    rhdf5::h5writeDataset.data.frame(obj=low[[x]],
                              h5loc=low.outloc,
                              name=x,
                              DataFrameAsCompound = TRUE)})
  
  rhdf5::H5Gclose(low.outloc)

  #------------------------------------------------------------
  #medium
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/data/isoCo2/co2Med_09m'))

  med.outloc <- rhdf5::H5Gopen(fid,paste0('/',site,'/dp01/data/isoCo2/co2Med_09m'))
  
  med <- rhdf5::h5read(inname,paste0('/',site,'/dp01/data/isoCo2/co2Med_09m'))
  
  # kludge fix for now - need to add mean_cal column to low - but currently uncalibrated!
  med$dlta13CCo2$mean_cal <- med$dlta13CCo2$mean
  med$dlta13CCo2$mean_cal <- as.numeric(NA)
  
  med$rtioMoleDryCo2$mean_cal <- med$rtioMoleDryCo2$mean
  med$rtioMoleDryCo2$mean_cal <- as.numeric(NA)
  
  # loop through each of the variables in list amb.data.list and write out as a dataframe.
  lapply(names(med),function(x) {
    rhdf5::h5writeDataset.data.frame(obj=med[[x]],
                              h5loc=med.outloc,
                              name=x,
                              DataFrameAsCompound = TRUE)})
 
  rhdf5::H5Gclose(med.outloc)

  #------------------------------------------------------------
  #high
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/data/isoCo2/co2High_09m'))

  high.outloc <- rhdf5::H5Gopen(fid,paste0('/',site,'/dp01/data/isoCo2/co2High_09m'))

  high <- rhdf5::h5read(inname,paste0('/',site,'/dp01/data/isoCo2/co2High_09m'))
  
  # kludge fix for now - need to add mean_cal column to low - but currently uncalibrated!
  high$dlta13CCo2$mean_cal <- high$dlta13CCo2$mean
  high$dlta13CCo2$mean_cal <- as.numeric(NA)
  
  high$rtioMoleDryCo2$mean_cal <- high$rtioMoleDryCo2$mean
  high$rtioMoleDryCo2$mean_cal <- as.numeric(NA)
  # loop through each of the variables in list amb.data.list and write out as a dataframe.
  lapply(names(high),function(x) {
    rhdf5::h5writeDataset.data.frame(obj=high[[x]],
                              h5loc=high.outloc,
                              name=x,
                              DataFrameAsCompound = TRUE)})
 
  rhdf5::H5Gclose(high.outloc)

  # close the group and the file
  rhdf5::H5Fclose(fid)
  Sys.sleep(0.5)
  
  rhdf5::h5closeAll()  
  #----------------------------------------------------------------------------------------
  # calibrate ambient data.
  # extract ambient measurements from ciso
  ciso_logical <- grepl(pattern="000",x=names(ciso))
  ciso_subset <- ciso[ciso_logical]

  if (filter.ambient == TRUE) {
    lapply(names(ciso_subset),
           function(x){calibrate_ambient_carbon_Bowling2003(amb.data.list=ciso_subset[[x]],
                                                            caldf=out,outname=x,file=outname,site=site,
                                                            filter.data=TRUE)})
  } else {
    lapply(names(ciso_subset),
           function(x){calibrate_ambient_carbon_Bowling2003(amb.data.list=ciso_subset[[x]],
                                                            caldf=out,outname=x,file=outname,site=site)})
  }


  rhdf5::h5closeAll()
  
  print("Copying qfqm...")
  # copy over ucrt and qfqm groups as well.
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/qfqm/'))
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/qfqm/isoCo2'))
  qfqm <- rhdf5::h5read(inname,paste0('/',site,'/dp01/qfqm/isoCo2'))

  lapply(names(qfqm),function(x) {
    copy_qfqm_group(data.list=qfqm[[x]],
                    outname=x,file=outname,site=site,species="CO2")})

  rhdf5::h5closeAll()

  print("Copying ucrt...")
  # now ucrt.
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/ucrt/'))
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/ucrt/isoCo2'))
  ucrt <- rhdf5::h5read(inname,paste0('/',site,'/dp01/ucrt/isoCo2'))

  lapply(names(ucrt),function(x) {
    copy_ucrt_group(data.list=ucrt[[x]],
                    outname=x,file=outname,site=site,species="CO2")})

  rhdf5::h5closeAll()
  
  Sys.sleep(0.5)
}