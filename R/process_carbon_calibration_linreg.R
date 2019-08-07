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
  print("Applying method 2: three-point mixing ratio bracketing interpolation")

  ciso <- h5read(fname,paste0('/',site,'/dp01/data/isoCo2'))
  
  # extract standards data.
  high <- ciso$co2High_09m
  med <- ciso$co2Med_09m
  low <- ciso$co2Low_09m
  

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
  
  var_for_h5$start <- convert_POSIXct_to_NEONhdf5_time(out$start)
  var_for_h5$end <- convert_POSIXct_to_NEONhdf5_time(out$den)
  
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
  co2.cal.outloc <- H5Gopen(fid,paste0('/',site,'/dp01iso/data/isoCo2'))
  
  # write out dataset.
  h5writeDataset.data.frame(obj = var_for_h5,h5loc=co2.cal.outloc,name="calRegressions",DataFrameAsCompound = TRUE)
  
  # close the group and the file
  H5Gclose(co2.cal.outloc)
  H5Fclose(fid)
  h5closeAll()  
  
  # calibrate data for each height.
  #-------------------------------------
  # extract ambient measurements from ciso
  ciso_logical <- grepl(pattern="000",x=names(ciso))
  ciso_subset <- ciso[ciso_logical]
  
  lapply(names(ciso_subset),
         function(x){calibrate_ambient_carbon_linreg(amb.data.list=ciso_subset[[x]],
                                                          caldf=out,outname=x,file=outname,site=site)})

}