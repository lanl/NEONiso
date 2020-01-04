#' calibrate_water_linreg
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param site Four-letter NEON code for site being processed.
#' @param time.diff.betweeen.standards Time (in seconds) required between consecutive standard measurements.
#' @param inname Name of the input file.
#' @param outname Name of the output file.
#' @param force.cal.to.beginning Extend first calibration to the beginning of the file? (Default true)
#' @param force.cal.to.end Extend last calibration to the end of the file? (Detault true)
#'
#' @return
#' @export
#'
#' @examples
calibrate_water_linreg <- function(inname,outname,site,time.diff.betweeen.standards=1800,
                                           force.cal.to.beginning=TRUE,force.cal.to.end=TRUE){
  
  # list required packages.
  require(rhdf5)
  require(dplyr)
  
  # print status.
  print("Processing water calibration data...")
  
  # load file, get calibration data.
  wiso <- h5read(inname,paste0('/',site,'/dp01/data/isoH2o'))
  
  # extract standards data.
  high <- wiso$h2oHigh_03m
  med <- wiso$h2oMed_03m
  low <- wiso$h2oLow_03m
  
  # attempt to pull relevent data out to a single dataframe.
  high_rs <- data.frame(d18O_meas_mean=high$dlta18OH2o$mean,
                        d18O_meas_var=high$dlta18OH2o$vari,
                        d18O_meas_n=high$dlta18OH2o$numSamp,
                        d18O_meas_btime=high$dlta18OH2o$timeBgn,
                        d18O_meas_etime=high$dlta18OH2o$timeEnd,
                        d18O_ref_mean=high$dlta18OH2oRefe$mean,
                        d18O_ref_var=high$dlta18OH2oRefe$vari,
                        d18O_ref_n=high$dlta18OH2oRefe$numSamp,
                        d18O_ref_btime=high$dlta18OH2oRefe$timeBgn,
                        d18O_ref_etime=high$dlta18OH2oRefe$timeEnd,
                        d2H_meas_mean=high$dlta2HH2o$mean,
                        d2H_meas_var=high$dlta2HH2o$vari,
                        d2H_meas_n=high$dlta2HH2o$numSamp,
                        d2H_meas_btime=high$dlta2HH2o$timeBgn,
                        d2H_meas_etime=high$dlta2HH2o$timeEnd,
                        d2H_ref_mean=high$dlta2HH2oRefe$mean,
                        d2H_ref_var=high$dlta2HH2oRefe$vari,
                        d2H_ref_n=high$dlta2HH2oRefe$numSamp,
                        d2H_ref_btime=high$dlta2HH2oRefe$timeBgn,
                        d2H_ref_etime=high$dlta2HH2oRefe$timeEnd)
  
  high_rs <- high_rs %>%
    mutate(std_name="high")
  
  med_rs <- data.frame(d18O_meas_mean=med$dlta18OH2o$mean,
                       d18O_meas_var=med$dlta18OH2o$vari,
                       d18O_meas_n=med$dlta18OH2o$numSamp,
                       d18O_meas_btime=med$dlta18OH2o$timeBgn,
                       d18O_meas_etime=med$dlta18OH2o$timeEnd,
                       d18O_ref_mean=med$dlta18OH2oRefe$mean,
                       d18O_ref_var=med$dlta18OH2oRefe$vari,
                       d18O_ref_n=med$dlta18OH2oRefe$numSamp,
                       d18O_ref_btime=med$dlta18OH2oRefe$timeBgn,
                       d18O_ref_etime=med$dlta18OH2oRefe$timeEnd,
                       d2H_meas_mean=med$dlta2HH2o$mean,
                       d2H_meas_var=med$dlta2HH2o$vari,
                       d2H_meas_n=med$dlta2HH2o$numSamp,
                       d2H_meas_btime=med$dlta2HH2o$timeBgn,
                       d2H_meas_etime=med$dlta2HH2o$timeEnd,
                       d2H_ref_mean=med$dlta2HH2oRefe$mean,
                       d2H_ref_var=med$dlta2HH2oRefe$vari,
                       d2H_ref_n=med$dlta2HH2oRefe$numSamp,
                       d2H_ref_btime=med$dlta2HH2oRefe$timeBgn,
                       d2H_ref_etime=med$dlta2HH2oRefe$timeEnd)
  
  med_rs <- med_rs %>%
    mutate(std_name="med")
  
  low_rs <- data.frame(d18O_meas_mean=low$dlta18OH2o$mean,
                       d18O_meas_var=low$dlta18OH2o$vari,
                       d18O_meas_n=low$dlta18OH2o$numSamp,
                       d18O_meas_btime=low$dlta18OH2o$timeBgn,
                       d18O_meas_etime=low$dlta18OH2o$timeEnd,
                       d18O_ref_mean=low$dlta18OH2oRefe$mean,
                       d18O_ref_var=low$dlta18OH2oRefe$vari,
                       d18O_ref_n=low$dlta18OH2oRefe$numSamp,
                       d18O_ref_btime=low$dlta18OH2oRefe$timeBgn,
                       d18O_ref_etime=low$dlta18OH2oRefe$timeEnd,
                       d2H_meas_mean=low$dlta2HH2o$mean,
                       d2H_meas_var=low$dlta2HH2o$vari,
                       d2H_meas_n=low$dlta2HH2o$numSamp,
                       d2H_meas_btime=low$dlta2HH2o$timeBgn,
                       d2H_meas_etime=low$dlta2HH2o$timeEnd,
                       d2H_ref_mean=low$dlta2HH2oRefe$mean,
                       d2H_ref_var=low$dlta2HH2oRefe$vari,
                       d2H_ref_n=low$dlta2HH2oRefe$numSamp,
                       d2H_ref_btime=low$dlta2HH2oRefe$timeBgn,
                       d2H_ref_etime=low$dlta2HH2oRefe$timeEnd)
  
  low_rs <- low_rs %>%
    mutate(std_name="low")
  
  # add fix for NEON standard swap.
  low_rs <- swap_standard_isotoperatios(low_rs)
  
  #--------------------------------------------------------------
  # Ensure there are the same number of standard measurements for each standard.
  #--------------------------------------------------------------
  
  # 191024 rpf - prior versions of this have just sliced out the first observation per day.
  # however, the most common cause of multiple standards to be analyzed per day is a 
  # malfunctioning valve in the manifold that causes the same standard gas to register as multiple
  # peaks. each peak is shorter, higher variance, and doesn't allow even the CO2 concentration
  # to stabilize. until further notice, i suggest removing these standards altogether.
  # code below has been modified to achieve this.
  # 200103 rpf - copying over this code from carbon script to fix the same bug present in 
  # the water isotope code. modify slightly to account for the fact that we expect more than
  # 1 row per day. commented out 
  
  high_rs <- high_rs %>%
    mutate(dom = day(d18O_meas_btime)) %>% # get day of month
    group_by(dom) %>%
    filter(d18O_meas_n > 30 | is.na(d18O_meas_n)) %>% # check to make sure peak sufficiently long, then slice off single.
    slice(tail(row_number(),3)) %>%
    ungroup()
  
  med_rs <- med_rs %>%
    mutate(dom = day(d18O_meas_btime)) %>% # get day of month
    group_by(dom) %>%
    filter(d18O_meas_n > 30 | is.na(d18O_meas_n)) %>% # check to make sure peak sufficiently long, then slice off single.
    slice(tail(row_number(),3)) %>%
    ungroup()
  
  low_rs <- low_rs %>%
    mutate(dom = day(d18O_meas_btime)) %>% # get day of month
    group_by(dom) %>%
    filter(d18O_meas_n > 30 | is.na(d18O_meas_n)) %>% # check to make sure peak sufficiently long, then slice off single.
    slice(tail(row_number(),3)) %>%
    ungroup()
  
  # bind together, and cleanup.
  stds <- do.call(rbind,list(high_rs,med_rs,low_rs))
  
  #rm(high_rs,med_rs,low_rs,high,med,low)
  
  # replace NaNs with NA
  # rpf note on 181121 - what does this line actually do? Seems tautological.
  # rpf note 181126 - is.na() also returns NaN as NA, so this does actually do what first
  # comment indicates.
  stds[ is.na(stds) ] <- NA
  
  #-----------------------------------------------------------
  # CALIBRATE WATER ISOTOPE VALUES
  
  # change class of time variables from charatcter to posixct.
  stds$d18O_meas_btime <- as.POSIXct(stds$d18O_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  stds$d18O_meas_etime <- as.POSIXct(stds$d18O_meas_etime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  
  stds$d18O_ref_btime <- as.POSIXct(stds$d18O_ref_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  stds$d18O_ref_etime <- as.POSIXct(stds$d18O_ref_etime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  
  stds$d2H_meas_btime <- as.POSIXct(stds$d2H_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  stds$d2H_meas_etime <- as.POSIXct(stds$d2H_meas_etime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  
  stds$d2H_ref_btime <- as.POSIXct(stds$d2H_ref_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  stds$d2H_ref_etime <- as.POSIXct(stds$d2H_ref_etime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
  
  # reorder data frame
  stds <- stds[order(stds$d18O_meas_btime),]
  
  # assign a vector corresponding to calibration period.
  stds$cal_period <- stds$d18O_meas_n
  
  period_id <- 1
  tdiffs <- c(diff(stds$d18O_meas_btime),0)
  for (i in 1:nrow(stds)) {
    stds$cal_period[i] <- period_id
    
    if (tdiffs[i] >= time.diff.betweeen.standards) {period_id = period_id + 1}
  }
  
  # okay, now run calibrations...
  #------------------------------
  
  # create output variables.
  oxy_cal_slopes <- vector()
  oxy_cal_ints   <- vector()
  oxy_cal_rsq    <- vector()
  
  hyd_cal_slopes <- vector()
  hyd_cal_ints   <- vector()
  hyd_cal_rsq    <- vector()
  
  for (i in 2:max(stds$cal_period)) {
    # check to see if data exist.
    cal.subset <- stds[which(stds$cal_period==i | stds$cal_period==(i-1)),]
    
    # check to see if sum of is.na() on oxygen data = nrow of oxygen data
    if (sum(is.na(cal.subset$d18O_meas_mean)) < nrow(cal.subset) &
        sum(is.na(cal.subset$d18O_ref_mean)) < nrow(cal.subset)) {
      tmp <- lm(d18O_ref_mean ~ d18O_meas_mean,data=cal.subset)
      
      oxy_cal_slopes[i-1] <- coef(tmp)[[2]]
      oxy_cal_ints[i-1] <- coef(tmp)[[1]]
      oxy_cal_rsq[i-1] <- summary(tmp)$r.squared  
      
    } else { # all are missing
      oxy_cal_slopes[i-1] <- NA
      oxy_cal_ints[i-1] <- NA
      oxy_cal_rsq[i-1] <- NA 
    }
    
    # HYDROGEN
    
    # check to see if sum of is.na() on oxygen data = nrow of oxygen data
    if (sum(is.na(cal.subset$d2H_meas_mean)) < nrow(cal.subset) &
        sum(is.na(cal.subset$d2H_ref_mean)) < nrow(cal.subset)) {
      tmp <- lm(d2H_ref_mean ~ d2H_meas_mean,data=cal.subset)
      
      hyd_cal_slopes[i-1] <- coef(tmp)[[2]]
      hyd_cal_ints[i-1] <- coef(tmp)[[1]]
      hyd_cal_rsq[i-1] <- summary(tmp)$r.squared  
      
    } else { # all are missing
      hyd_cal_slopes[i-1] <- NA
      hyd_cal_ints[i-1] <- NA
      hyd_cal_rsq[i-1] <- NA 
    }
  }
  
  # make dataframe of calibration data.
  # TODO: add hydrogen data in here...
  times <- stds %>%
    select(d18O_meas_btime,d18O_meas_etime,d18O_ref_btime,d18O_ref_etime,
           d2H_meas_btime,d2H_meas_etime,d2H_ref_btime,d2H_ref_etime,cal_period) %>%
    group_by(cal_period) %>%
    summarize(etime = max(c(d18O_meas_etime,d18O_ref_etime,d2H_meas_etime,d2H_ref_etime)))
  
  # loop through times, assign beginning, ending value. max etime should be just fine.
  starttimes <- vector()
  endtimes <- vector()
  
  for (i in 1:length(oxy_cal_slopes)) {
    starttimes[i] <- times$etime[i]
    endtimes[i] <- times$etime[i+1]
  }
  
  # output dataframe giving valid time range, slopes, intercepts, rsquared.
  out <- data.frame(start=as.POSIXct(starttimes,tz="UTC",origin="1970-01-01"),
                    end=as.POSIXct(endtimes,tz="UTC",origin="1970-01-01"),
                    o.slope=oxy_cal_slopes,o.intercept=oxy_cal_ints,o.r2=oxy_cal_rsq,
                    h.slope=hyd_cal_slopes,h.intercept=hyd_cal_ints,h.r2=hyd_cal_rsq)
  
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
  h5createFile(outname)
  h5createGroup(outname,paste0('/',site))
  h5createGroup(outname,paste0('/',site,'/dp01'))
  h5createGroup(outname,paste0('/',site,'/dp01/data'))
  h5createGroup(outname,paste0('/',site,'/dp01/data/isoH2o'))
  
  # okay try to write out to h5 file.
  fid <- H5Fopen(outname)
  
  h2o.cal.outloc <- H5Gopen(fid,paste0('/',site,'/dp01/data/isoH2o'))
  
  # write out dataset.
  h5writeDataset.data.frame(obj = var_for_h5,h5loc=h2o.cal.outloc,name="calRegressions",DataFrameAsCompound = TRUE)
  
  # close the group and the file
  H5Gclose(h2o.cal.outloc)
  
  #-----------------------------------------
  # write out high/mid/low rs.
  
  #low
  h5createGroup(outname,paste0('/',site,'/dp01/data/isoH2o/h2oLow_09m'))

  low.outloc <- H5Gopen(fid,paste0('/',site,'/dp01/data/isoH2o/h2oLow_09m'))

  # check to see if there are any data; if not, fill w/ row of NAs.
  if (nrow(low_rs) < 1) {
    low_rs[1,] <- rep(NA,ncol(low_rs))
  }

  h5writeDataset.data.frame(obj = low_rs,h5loc=low.outloc,
                            name="wisoStds",
                            DataFrameAsCompound = TRUE)

  H5Gclose(low.outloc)

  #------------------------------------------------------------
  #medium
  h5createGroup(outname,paste0('/',site,'/dp01/data/isoH2o/h2oMed_09m'))

  med.outloc <- H5Gopen(fid,paste0('/',site,'/dp01/data/isoH2o/h2oMed_09m'))

  if (nrow(med_rs) < 1) {
    med_rs[1,] <- rep(NA,ncol(med_rs))
  }

  h5writeDataset.data.frame(obj = med_rs,h5loc=med.outloc,
                            name="wisoStds",
                            DataFrameAsCompound = TRUE)

  H5Gclose(med.outloc)

  #------------------------------------------------------------
  #high

  h5createGroup(outname,paste0('/',site,'/dp01/data/isoH2o/h2oHigh_09m'))

  high.outloc <- H5Gopen(fid,paste0('/',site,'/dp01/data/isoH2o/h2oHigh_09m'))

  if (nrow(high_rs) < 1) {
    high_rs[1,] <- rep(NA,ncol(med_rs))
  }

  h5writeDataset.data.frame(obj = high_rs,h5loc=high.outloc,
                            name="wisoStds",
                            DataFrameAsCompound = TRUE)
  H5Gclose(high.outloc)

  # close the group and the file
  H5Fclose(fid)
  Sys.sleep(0.5)

  h5closeAll()
  
  #===========================================================
  # calibrate data for each height.
  #-------------------------------------
  # extract ambient measurements from ciso
  wiso_logical <- grepl(pattern="000",x=names(wiso))
  wiso_subset <- wiso[wiso_logical]

  lapply(names(wiso_subset),
         function(x){calibrate_ambient_water_linreg(amb.data.list=wiso_subset[[x]],
                                                     caldf=out,outname=x,file=outname,site=site)})

  h5closeAll()


  print("Copying qfqm...")
  # copy over ucrt and qfqm groups as well.
  h5createGroup(outname,paste0('/',site,'/dp01/qfqm/'))
  h5createGroup(outname,paste0('/',site,'/dp01/qfqm/isoH2o'))
  qfqm <- h5read(inname,paste0('/',site,'/dp01/qfqm/isoH2o'))

  lapply(names(qfqm),function(x) {
    copy_qfqm_group(data.list=qfqm[[x]],
                    outname=x,file=outname,site=site,species="H2O")})

  h5closeAll()

  print("Copying ucrt...")
  # now ucrt.
  h5createGroup(outname,paste0('/',site,'/dp01/ucrt/'))
  h5createGroup(outname,paste0('/',site,'/dp01/ucrt/isoH2o'))
  ucrt <- h5read(inname,paste0('/',site,'/dp01/ucrt/isoH2o'))

  lapply(names(ucrt),function(x) {
    copy_ucrt_group(data.list=ucrt[[x]],
                    outname=x,file=outname,site=site,species="H2O")})

  h5closeAll()
   
}