#' calibrate_carbon_linreg
#' 
#' This function will calibrate NEON carbon isotope data using an ordinary
#' least squares linear regression between measured and reference d13C values.
#' Correction equation is determined by regressing the reference values on 
#' the measured values, and uses this equation to calibrate the ambient data.
#' In brief, this function takes the following steps:
#' \enumerate{
#'   \item Extracts calibration data from uncalibrated file.
#'   \item Basic QA/QC on each calibration data point, where the following factors must be true:
#'   \itemize{
#'     \item Calibration "peak" must have >= 200 data points, to remove some observed issues with gas manifold valves.
#'     \item Calibration "peak" must not be missing.
#'     \item Only one value per day meeting these criteria are selected.
#'   }
#'   \item Calibration periods are defined to bracket a sample of ambient data. In many cases, this will be one day bracketed by 
#'         measurements of reference materials immediately before and immediately after this period.
#'   \item Determine the slope, intercept, and r^2 of a regression of each calibration period. Calibration error is estimated using
#'         the difference between the model-predicted reference value of each standard compared to the "known" reference value 
#'         for the "medium" standard only. As this bracketing calibration may use two measurements of the medium standard, only the maximum
#'         difference value is retained as the more conservative approach. The "valid" period of the regression is determined as the
#'         
#'   \item Regression parameters are written to a dataset in a new output file under \code{/site/dp01/data/isoCo2/calData/calRegressions}
#'   \item Regression equations are applied ambient data, and written to same new output file.
#' }
#' The qfqm and ucrt folders are also copied over from the original file, and are unchanged.
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param site Four-letter NEON code for site being processed. (character)
#' @param inname Name of the input file. (character)
#' @param outname Name of the output file. (character)
#' @param force.cal.to.beginning Extend first calibration to the beginning of the file? (CURRENTLY NOT USED)
#' @param time.diff.betweeen.standards Time (in seconds) required between consecutive standard measurements. Used to define a calibration "period."
#' @param force.cal.to.end Extend last calibration to the end of the file? (CURRENTLY NOT USED)
#'
#' @return nothing to the workspace, but creates a new output file of calibrated carbon isotope data.
#' @export
#' 
#' @importFrom magrittr %>%

calibrate_carbon_linreg <- function(inname,
                                    outname,
                                    site,
                                    time.diff.betweeen.standards=1800,
                                    force.cal.to.beginning=TRUE,
                                    force.cal.to.end=TRUE){
  
  # print status.
  print("Processing carbon calibration data...")
  print("Applying three-point mixing ratio bracketing interpolation")

  ciso <- rhdf5::h5read(inname,paste0('/',site,'/dp01/data/isoCo2'))
  ucrt <- rhdf5::h5read(inname,paste0('/',site,'/dp01/ucrt/isoCo2'))
  
  high_rs <- extract_carbon_calibration_data(ciso,ucrt,"high")
  med_rs  <- extract_carbon_calibration_data(ciso,ucrt,"med")
  low_rs  <- extract_carbon_calibration_data(ciso,ucrt,"low")
  
  # cut out period where there appears to be a valve malfunction.
  high_rs <- high_rs %>%
    dplyr::mutate(dom = lubridate::day(d13C_obs_btime)) %>%
    dplyr::group_by(dom) %>%
    dplyr::filter(d13C_obs_n > 200 | is.na(d13C_obs_n)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  med_rs <- med_rs %>%
    dplyr::mutate(dom = lubridate::day(d13C_obs_btime)) %>%
    dplyr::group_by(dom) %>%
    dplyr::filter(d13C_obs_n > 200 | is.na(d13C_obs_n)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()  
  
  low_rs <- low_rs %>%
    dplyr::mutate(dom = lubridate::day(d13C_obs_btime)) %>%
    dplyr::group_by(dom) %>%
    dplyr::filter(d13C_obs_n > 200 | is.na(d13C_obs_n)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  #=======================================================================
  # apply calibration routines
  #======================================================================= 
  # bind together, and cleanup.
  stds <- do.call(rbind,list(high_rs,med_rs,low_rs))
  
  if (nrow(stds) > 0) {
    # replace NaNs with NA
    # rpf note on 181121 - what does this line actually do? Seems tautological.
    # rpf note 181126 - is.na() also returns NaN as NA, so this does actually do what first
    # comment indicates.
    stds[ is.na(stds) ] <- NA
    
    # change class of time variables from charatcter to posixct.
    stds$d13C_obs_btime <- as.POSIXct(stds$d13C_obs_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    stds$d13C_obs_etime <- as.POSIXct(stds$d13C_obs_etime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    
    stds$d13C_ref_btime <- as.POSIXct(stds$d13C_ref_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    stds$d13C_ref_etime <- as.POSIXct(stds$d13C_ref_etime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    
    # reorder data frame
    stds <- stds[order(stds$d13C_obs_btime),]
    
    # assign a vector corresponding to calibration period.
    stds$cal_period <- stds$d13C_obs_n
    
    period_id <- 1
    tdiffs <- c(diff(stds$d13C_obs_btime),0)
    for (i in 1:nrow(stds)) {
      stds$cal_period[i] <- period_id   
      if (tdiffs[i] >= time.diff.betweeen.standards) {period_id = period_id + 1}
    }
    
    # okay, now run calibrations...
    #------------------------------
    # create output variables.
    delta_cal_slopes <- vector()
    delta_cal_ints   <- vector()
    delta_cal_rsq    <- vector()
    calDelUcrt    <- vector()
    
    co2_cal_slopes <- vector()
    co2_cal_ints   <- vector()
    co2_cal_rsq    <- vector()
    calCO2Ucrt    <- vector()
    
    for (i in 2:max(stds$cal_period)) {
      # subset data.
      cal.subset <- stds[which(stds$cal_period==i | stds$cal_period==(i-1)),]
      
      #---------------------------------------------
      # do some light validation of these points.
      cal.subset <- cal.subset %>%
        dplyr::filter(d13C_obs_var < 5 & abs(CO2_obs_mean - CO2_ref_mean) < 15)
      
      if (length(unique(cal.subset$std_name)) >= 2 & # ensure at least 2 standards are present
          !all(is.na(cal.subset$d13C_obs_mean)) & # ensure that not all observational values are missing
          !all(is.na(cal.subset$d13C_ref_mean))) { # ensure that not all reference values are missing.
        
        # model to calibrate delta 13C values.
        tmpmod <- lm(d13C_ref_mean ~ d13C_obs_mean,data=cal.subset)
        
        delta_cal_slopes[i-1] <- coef(tmpmod)[[2]]
        delta_cal_ints[i-1] <- coef(tmpmod)[[1]]
        delta_cal_rsq[i-1] <- summary(tmpmod)$r.squared
        
        # model to calibrate delta 13C values.
        tmpmod <- lm(CO2_ref_mean ~ CO2_obs_mean,data=cal.subset)
        
        co2_cal_slopes[i-1] <- coef(tmpmod)[[2]]
        co2_cal_ints[i-1] <- coef(tmpmod)[[1]]
        co2_cal_rsq[i-1] <- summary(tmpmod)$r.squared
        
        if (any(cal.subset$std_name == "med")) {
          # get medium vars.
          tmp <- subset(cal.subset, std_name == "med")
          
          calDelUcrt[i-1] <- max(tmp$d13C_obs_mean*delta_cal_slopes[i-1] + delta_cal_ints[i-1] - tmp$d13C_ref_mean,
                                  na.rm = TRUE)
          calCO2Ucrt[i-1] <- max(tmp$CO2_obs_mean*co2_cal_slopes[i-1] + co2_cal_ints[i-1] - tmp$CO2_ref_mean,
                                 na.rm = TRUE)
          
        } else {
          
          calDelUcrt[i-1] <- NA
          calCO2Ucrt[i-1] <- NA
          
        }
        
      } else {
        
        delta_cal_slopes[i-1] <- NA
        delta_cal_ints[i-1] <- NA
        delta_cal_rsq[i-1] <- NA
        calDelUcrt[i-1] <- NA
        
        co2_cal_slopes[i-1] <- NA
        co2_cal_ints[i-1] <- NA
        co2_cal_rsq[i-1] <- NA
        calCO2Ucrt[i-1] <- NA
                
      }
    }
    
    # make dataframe of calibration data.
    times <- stds %>%
      dplyr::select(d13C_obs_btime,d13C_obs_etime,d13C_ref_btime,d13C_ref_etime,cal_period) %>%
      dplyr::group_by(cal_period) %>%
      dplyr::summarize(etime = max(c(d13C_obs_etime,d13C_ref_etime)))
    
    # loop through times, assign beginning, ending value. max etime should be just fine.
    starttimes <- vector()
    endtimes <- vector()
    
    for (i in 1:length(delta_cal_slopes)) {
      starttimes[i] <- times$etime[i]
      endtimes[i] <- times$etime[i+1]
    }
    
    # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start=as.POSIXct(starttimes,tz="UTC",origin="1970-01-01"),
                      end=as.POSIXct(endtimes,tz="UTC",origin="1970-01-01"),
                      d13C_slope=delta_cal_slopes,d13C_intercept=delta_cal_ints,d13C_r2=delta_cal_rsq,
                      co2_slope=co2_cal_slopes,co2_intercept=co2_cal_ints,co2_r2=co2_cal_rsq,
                      calDelUcrt=as.numeric(calDelUcrt),
                      calCO2Ucrt=as.numeric(calCO2Ucrt))
    
  } else {

        # output dataframe giving valid time range, slopes, intercepts, rsquared.
    out <- data.frame(start=as.POSIXct(as.Date("1970-01-01"),tz="UTC",origin="1970-01-01"),
                      end=as.POSIXct(as.Date("1970-01-01"),tz="UTC",origin="1970-01-01"),
                      d13C_slope=as.numeric(NA),d13C_intercept=as.numeric(NA),d13C_r2=as.numeric(NA),
                      co2_slope=as.numeric(NA),co2_intercept=as.numeric(NA),co2_r2=as.numeric(NA),
                      calDelUcrt=as.numeric(NA),calCO2Ucrt=as.numeric(NA))
  }
  
  # check to ensure there are 6 columns.   
  # add slope, intercept, r2 columns if missing.
  if (!("d13C_slope" %in% names(out))) {
    out$d13C_slope <- as.numeric(rep(NA,length(out$start)))
  }
  if (!("d13C_intercept" %in% names(out))) {
    out$d13C_intercept <- as.numeric(rep(NA,length(out$start)))
  }
  if (!("d13C_r2" %in% names(out))) {
    out$d13C_r2 <- as.numeric(rep(NA,length(out$start)))
  }  
  if (!("co2_slope" %in% names(out))) {
    out$co2_slope <- as.numeric(rep(NA,length(out$start)))
  }
  if (!("co2_intercept" %in% names(out))) {
    out$co2_intercept <- as.numeric(rep(NA,length(out$start)))
  }
  if (!("co2_r2" %in% names(out))) {
    out$co2_r2 <- as.numeric(rep(NA,length(out$start)))
  }
 
  print(ncol(out))
  
  var_for_h5 <- out
  
  var_for_h5$start <- convert_POSIXct_to_NEONhdf5_time(out$start)
  var_for_h5$end   <- convert_POSIXct_to_NEONhdf5_time(out$end)
  
  var_for_h5$valid_period_start <- var_for_h5$start
  var_for_h5$valid_period_end   <- var_for_h5$end
  
  # some columns are stripped when written out to file 
  # I think there was a similar issue w/ the Bowling calibration that 
  # was solved by enforcing all numeric coolumns to be numeric.  
  # *feels super redundant w/ above, but worth a shot i suppose*
  var_for_h5$d13C_slope <- as.numeric(var_for_h5$d13C_slope)
  var_for_h5$co2_slope <- as.numeric(var_for_h5$co2_slope)  
  var_for_h5$d13C_intercept <- as.numeric(var_for_h5$d13C_intercept)
  var_for_h5$co2_intercept <- as.numeric(var_for_h5$co2_intercept)
  var_for_h5$d13C_r2 <- as.numeric(var_for_h5$d13C_r2)
  var_for_h5$co2_r2 <- as.numeric(var_for_h5$co2_r2)
  
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
  
  rhdf5::h5createGroup(outname,paste0('/',site,'/dp01/data/isoCo2/calData'))
  co2.cal.outloc <- rhdf5::H5Gopen(fid,paste0('/',site,'/dp01/data/isoCo2/calData'))
  
  # write out dataset.
  rhdf5::h5writeDataset.data.frame(obj = var_for_h5,h5loc=co2.cal.outloc,name="calRegressions",DataFrameAsCompound = TRUE)
  
  # close the group and the file
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
  
  
  # calibrate data for each height.
  #-------------------------------------
  # extract ambient measurements from ciso
  ciso_logical <- grepl(pattern="000",x=names(ciso))
  ciso_subset <- ciso[ciso_logical]
  
  lapply(names(ciso_subset),
         function(x){calibrate_ambient_carbon_linreg(amb.data.list=ciso_subset[[x]],
                                                          caldf=out,outname=x,file=outname,site=site)})
  
  rhdf5::h5closeAll()

  # copy over qfqm and ucrt data groups.
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
  
}