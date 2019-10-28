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
  high_rs <- data.frame(d13C_obs_mean=high$dlta13CCo2$mean,
                        d13C_obs_var=high$dlta13CCo2$vari,
                        d13C_obs_n=high$dlta13CCo2$numSamp,
                        d13C_obs_btime=high$dlta13CCo2$timeBgn,
                        d13C_obs_etime=high$dlta13CCo2$timeEnd,
                        CO2_obs_conc=high$rtioMoleDryCo2$mean,
                        CO2_obs_var=high$rtioMoleDryCo2$vari,
                        d13C_ref_mean=high$dlta13CCo2Refe$mean,
                        d13C_ref_var=high$dlta13CCo2Refe$vari,
                        d13C_ref_n=high$dlta13CCo2Refe$numSamp,
                        d13C_ref_btime=high$dlta13CCo2Refe$timeBgn,
                        d13C_ref_etime=high$dlta13CCo2Refe$timeEnd,
                        CO2_ref_conc=high$rtioMoleDryCo2Refe$mean,
                        CO2_ref_var=high$rtioMoleDryCo2Refe$vari)
  
  # calculate 12CO2 and 13CO2 concentrations for high standard
  # for reference and observed isotope ratios
  high_rs <- high_rs %>%
    mutate(std_name="high") %>%
    mutate(conc12CCO2_ref = CO2_ref_conc*(1-f)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
    mutate(conc13CCO2_ref = CO2_ref_conc*(1-f)-conc12CCO2_ref) %>%
    mutate(conc12CCO2_obs = CO2_obs_conc*(1-f)/(1+R_vpdb*(1+d13C_obs_mean/1000))) %>%
    mutate(conc13CCO2_obs = CO2_obs_conc*(1-f)-conc12CCO2_obs) %>%
    mutate(vari12CCO2_ref = 0.5) %>% # placeholder! need to figure out better solution.
    mutate(vari13CCO2_ref = 0.5) %>% # placeholder! need to figure out better solution.
    mutate(vari12CCO2_obs = ((1-f)/(1+R_vpdb*(d13C_obs_mean/1000+1)))^2*CO2_obs_var + 
                              ((1-f)*CO2_obs_mean/(1+R_vpdb*(d13C_obs_mean/1000+1))^2)^2*(R_vpdb/1000)^2*d13C_obs_var) %>%
    mutate(vari13CCO2_obs = (1-f)^2*CO2_obs_var + vari12CCO2_obs) %>%
    mutate(d13C_meas_btime=as.POSIXct(d13C_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")) # for assigning times later.
  
  # "medium" standard
  med_rs <- data.frame(d13C_obs_mean=med$dlta13CCo2$mean,
                       d13C_obs_var=med$dlta13CCo2$vari,
                       d13C_obs_n=med$dlta13CCo2$numSamp,
                       d13C_obs_btime=med$dlta13CCo2$timeBgn,
                       d13C_obs_etime=med$dlta13CCo2$timeEnd,
                       CO2_obs_conc=med$rtioMoleDryCo2$mean,
                       CO2_obs_var=med$rtioMoleDryCo2$vari,
                       d13C_ref_mean=med$dlta13CCo2Refe$mean,
                       d13C_ref_var=med$dlta13CCo2Refe$vari,
                       d13C_ref_n=med$dlta13CCo2Refe$numSamp,
                       d13C_ref_btime=med$dlta13CCo2Refe$timeBgn,
                       d13C_ref_etime=med$dlta13CCo2Refe$timeEnd,
                       CO2_ref_conc=med$rtioMoleDryCo2Refe$mean,
                       CO2_ref_var=high$rtioMoleDryCo2Refe$vari)
  
  # calculate 12CO2 and 13CO2 concentrations for medium standard
  # for reference and observed isotope ratios
  med_rs <- med_rs %>%
    mutate(std_name="med") %>%
    mutate(conc12CCO2_ref = CO2_ref_conc*(1-f)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
    mutate(conc13CCO2_ref = CO2_ref_conc*(1-f)-conc12CCO2_ref) %>%
    mutate(conc12CCO2_obs = CO2_obs_conc*(1-f)/(1+R_vpdb*(1+d13C_obs_mean/1000))) %>%
    mutate(conc13CCO2_obs = CO2_obs_conc*(1-f)-conc12CCO2_obs) %>%
    mutate(vari12CCO2_ref = 0.5) %>% # placeholder! need to figure out better solution.
    mutate(vari13CCO2_ref = 0.5) %>% # placeholder! need to figure out better solution.
    mutate(vari12CCO2_obs = ((1-f)/(1+R_vpdb*(d13C_obs_mean/1000+1)))^2*CO2_obs_var + 
             ((1-f)*CO2_obs_mean/(1+R_vpdb*(d13C_obs_mean/1000+1))^2)^2*(R_vpdb/1000)^2*d13C_obs_var) %>%
    mutate(vari13CCO2_obs = (1-f)^2*CO2_obs_var + vari12CCO2_obs) %>%
    mutate(d13C_meas_btime=as.POSIXct(d13C_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")) # for assigning times later.
  
  # "low" standard
  low_rs <- data.frame(d13C_obs_mean=low$dlta13CCo2$mean,
                       d13C_obs_var=low$dlta13CCo2$vari,
                       d13C_obs_n=low$dlta13CCo2$numSamp,
                       d13C_obs_btime=low$dlta13CCo2$timeBgn,
                       d13C_obs_etime=low$dlta13CCo2$timeEnd,
                       CO2_obs_conc=low$rtioMoleDryCo2$mean,
                       CO2_obs_var=low$rtioMoleDryCo2$vari,
                       d13C_ref_mean=low$dlta13CCo2Refe$mean,
                       d13C_ref_var=low$dlta13CCo2Refe$vari,
                       d13C_ref_n=low$dlta13CCo2Refe$numSamp,
                       d13C_ref_btime=low$dlta13CCo2Refe$timeBgn,
                       d13C_ref_etime=low$dlta13CCo2Refe$timeEnd,
                       CO2_ref_conc=low$rtioMoleDryCo2Refe$mean,
                       CO2_ref_var=high$rtioMoleDryCo2Refe$vari)
  
  # calculate 12CO2 and 13CO2 concentrations for low standard
  # for reference and observed isotope ratios
  low_rs <- low_rs %>%
    mutate(std_name="low") %>%
    mutate(conc12CCO2_ref = CO2_ref_conc*(1-f)/(1+R_vpdb*(1+d13C_ref_mean/1000))) %>%
    mutate(conc13CCO2_ref = CO2_ref_conc*(1-f)-conc12CCO2_ref) %>%
    mutate(conc12CCO2_obs = CO2_obs_conc*(1-f)/(1+R_vpdb*(1+d13C_obs_mean/1000))) %>%
    mutate(conc13CCO2_obs = CO2_obs_conc*(1-f)-conc12CCO2_obs) %>%
    mutate(vari12CCO2_ref = 0.5) %>% # placeholder! need to figure out better solution.
    mutate(vari13CCO2_ref = 0.5) %>% # placeholder! need to figure out better solution.
    mutate(vari12CCO2_obs = ((1-f)/(1+R_vpdb*(d13C_obs_mean/1000+1)))^2*CO2_obs_var + 
             ((1-f)*CO2_obs_mean/(1+R_vpdb*(d13C_obs_mean/1000+1))^2)^2*(R_vpdb/1000)^2*d13C_obs_var) %>%
    mutate(vari13CCO2_obs = (1-f)^2*CO2_obs_var + vari12CCO2_obs) %>%
    mutate(d13C_meas_btime=as.POSIXct(d13C_meas_btime,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")) # for assigning times later.
  
  # need to add propogation of uncertainty sometime! - RPF
  
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
      mutate(dom = day(d13C_meas_btime)) %>% # get day of month
      group_by(dom) %>%
      filter(d13C_meas_n > 250 | is.na(d13C_meas_n)) %>% # check to make sure peak sufficiently long, then slice off single.
      slice(1) %>%
      ungroup()
    
    med_rs <- med_rs %>%
      mutate(dom = day(d13C_meas_btime)) %>% # get day of month
      group_by(dom) %>%
      filter(d13C_meas_n > 250 | is.na(d13C_meas_n)) %>% # check to make sure peak sufficiently long, then slice off single.
      slice(1) %>%
      ungroup()
    
    low_rs <- low_rs %>%
      mutate(dom = day(d13C_meas_btime)) %>% # get day of month
      group_by(dom) %>%
      filter(d13C_meas_n > 250 | is.na(d13C_meas_n)) %>% # check to make sure peak sufficiently long, then slice off single.
      slice(1) %>%
      ungroup()

  # filter to only common days?
  common_days <- Reduce(intersect,list(low_rs$dom,med_rs$dom,high_rs$dom))
  
  low_rs <- low_rs %>%
    filter(dom %in% common_days)
  
  med_rs <- med_rs %>%
    filter(dom %in% common_days)
  
  high_rs <- high_rs %>%
    filter(dom %in% common_days)
      
  # if (!(identical(nrow(high_rs),nrow(med_rs)) & identical(nrow(high_rs),nrow(low_rs)))) {
  #   # if above logical evaluates as true, this means that the standards 
  #   # have a different number of observations.
  #   high_rs <- high_rs %>%
  #     mutate(dom = day(d13C_meas_btime)) %>% # get day of month
  #     group_by(dom) %>%
  #     slice(1) %>% # require group count to be 1 per note above.
  #     ungroup()
  
  #-------------------------------------------------------------------------------------
  # try to determine if all data points are valid. most obvious check here that 
  # should remove the most heinous values: are measured [CO2] w/in some tolerance
  # of expected [CO2]? This will help scrub out bad data from empty tanks, etc.
  
  conc_thres <- 10 # threshold in ppm.
  conc_var_thres <- 2 # threshold for co2 variance in ppm.
  
  # need to make a list of how many good calibration points there are for each calibration period.
  val.df <- data.frame(low=ifelse(abs(low_rs$CO2_meas_conc - low_rs$CO2_ref_conc) < conc_thres &
                                 low_rs$CO2_meas_var < conc_var_thres &
                                 !is.na(low_rs$d13C_meas_mean) & !is.na(low_rs$d13C_ref_mean),
                                  1,0), # 1 if true, 0 if false
                       med=ifelse(abs(med_rs$CO2_meas_conc - med_rs$CO2_ref_conc) < conc_thres &
                                    med_rs$CO2_meas_var < conc_var_thres &
                                    !is.na(med_rs$d13C_meas_mean) & !is.na(med_rs$d13C_ref_mean),
                                  1,0),
                       high=ifelse(abs(high_rs$CO2_meas_conc - high_rs$CO2_ref_conc) < conc_thres &
                                     high_rs$CO2_meas_var < conc_var_thres &
                                     !is.na(high_rs$d13C_meas_mean) & !is.na(high_rs$d13C_ref_mean),
                                  1,0))
  
  # add row sum.
  if (nrow(val.df) == 0) {
    val.df <- data.frame(low=NA,med=NA,high=NA)
  }
  
  val.df$tot <- rowSums(val.df,na.rm=TRUE) # make sure to remove NAs
  
  print(val.df)

  # there's almost definitely a faster way to implement this, but coding as a loop for now.
  #-----------------------------------------------------------------------
  # preallocate variables.
  gain12C <- gain13C <- vector(length = nrow(high_rs))
  offset12C <- offset13C <- vector(length = nrow(high_rs))
  vari.g12C <- vari.g13C <- vector(length = nrow(high_rs)) # sigma^2 !!!
  vari.o12C <- vari.o13C <- vector(length = nrow(high_rs)) # sigma^2 !!!
  
  for (i in 1:nrow(val.df)) {
    if (!is.na(val.df$tot[i]) & val.df$tot[i] == 3) { # e.g., all calibration points are good.
      # all points are good, so calibrate gain and offset w/ high and low points.
      gain12C[i] <- (high_rs$conc12CCO2_ref[i] - low_rs$conc12CCO2_ref[i])/(high_rs$conc12CCO2_obs[i] - low_rs$conc12CCO2_obs[i])
      gain13C[i] <- (high_rs$conc13CCO2_ref[i] - low_rs$conc13CCO2_ref[i])/(high_rs$conc13CCO2_obs[i] - low_rs$conc13CCO2_obs[i])
      
      offset12C[i] <- high_rs$conc12CCO2_ref[i] - gain12C[i]*high_rs$conc12CCO2_obs[i]
      offset13C[i] <- high_rs$conc13CCO2_ref[i] - gain13C[i]*high_rs$conc13CCO2_obs[i]
      
      # calculate uncertainties - standard propogation of uncertainty formula.
      vari.g12C[i] <- high_rs$vari12CCO2_obs[i]/(high_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^2 + 
        low_rs$vari12CCO2_obs[i]/(high_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^2 +
        high_rs$vari12CCO2_ref[i]/(high_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^4 +
        low_rs$vari12CCO2_ref[i]/(high_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^4
      
      vari.g13C[i] <- high_rs$vari13CCO2_obs[i]/(high_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^2 + 
        low_rs$vari13CCO2_obs[i]/(high_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^2 +
        high_rs$vari13CCO2_ref[i]/(high_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^4 +
        low_rs$vari13CCO2_ref[i]/(high_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^4
      
      vari.o12C[i] <- high_rs$vari12CCO2_ref[i] + high_rs$conc12CCO2_obs[i]^2*vari.g12C[i] + gain12C[i]^2*high_rs$vari12CCO2_obs[i]
      
      vari.o13C[i] <- high_rs$vari13CCO2_ref[i] + high_rs$conc13CCO2_obs[i]^2*vari.g13C[i] + gain13C[i]^2*high_rs$vari13CCO2_obs[i]
      
    } else if (!is.na(val.df$tot[i]) & val.df$tot[i] == 2) { # 1 calibration point doesn't pass test(s)
      # need to determine which two points are good, which can be done w/ 2 logical tests.
      
      if (!is.na(val.df$tot[i]) & !is.na(val.df$low[i]) & val.df$low[i] == 1) { # low point is good, need to determine if med or high point is
                                # other valid point.
        if (!is.na(val.df$tot[i]) & !is.na(val.df$med[i]) & val.df$med[i] == 1) { # low and medium point are valid.
          
          gain12C[i] <- (med_rs$conc12CCO2_ref[i] - low_rs$conc12CCO2_ref[i])/(med_rs$conc12CCO2_obs[i] - low_rs$conc12CCO2_obs[i])
          gain13C[i] <- (med_rs$conc13CCO2_ref[i] - low_rs$conc13CCO2_ref[i])/(med_rs$conc13CCO2_obs[i] - low_rs$conc13CCO2_obs[i])
          
          offset12C[i] <- med_rs$conc12CCO2_ref[i] - gain12C[i]*med_rs$conc12CCO2_obs[i]
          offset13C[i] <- med_rs$conc13CCO2_ref[i] - gain13C[i]*med_rs$conc13CCO2_obs[i] 
          
          # calculate uncertainties - standard propogation of uncertainty formula.
          vari.g12C[i] <- med_rs$vari12CCO2_obs[i]/(med_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^2 + 
            low_rs$vari12CCO2_obs[i]/(med_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^2 +
            med_rs$vari12CCO2_ref[i]/(med_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^4 +
            low_rs$vari12CCO2_ref[i]/(med_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^4
          
          vari.g13C[i] <- med_rs$vari13CCO2_obs[i]/(med_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^2 + 
            low_rs$vari13CCO2_obs[i]/(med_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^2 +
            med_rs$vari13CCO2_ref[i]/(med_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^4 +
            low_rs$vari13CCO2_ref[i]/(med_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^4
          
          vari.o12C[i] <- med_rs$vari12CCO2_ref[i] + med_rs$conc12CCO2_obs[i]^2*vari.g12C[i] + gain12C[i]^2*med_rs$vari12CCO2_obs[i]
          
          vari.o13C[i] <- med_rs$vari13CCO2_ref[i] + med_rs$conc13CCO2_obs[i]^2*vari.g13C[i] + gain13C[i]^2*med_rs$vari13CCO2_obs[i]
          
        } else { # low and high only are good.
        
          gain12C[i] <- (high_rs$conc12CCO2_ref[i] - low_rs$conc12CCO2_ref[i])/(high_rs$conc12CCO2_obs[i] - low_rs$conc12CCO2_obs[i])
          gain13C[i] <- (high_rs$conc13CCO2_ref[i] - low_rs$conc13CCO2_ref[i])/(high_rs$conc13CCO2_obs[i] - low_rs$conc13CCO2_obs[i])
          
          offset12C[i] <- high_rs$conc12CCO2_ref[i] - gain12C[i]*high_rs$conc12CCO2_obs[i]
          offset13C[i] <- high_rs$conc13CCO2_ref[i] - gain13C[i]*high_rs$conc13CCO2_obs[i]
          
          # calculate uncertainties - standard propogation of uncertainty formula.
          vari.g12C[i] <- high_rs$vari12CCO2_obs[i]/(high_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^2 + 
            low_rs$vari12CCO2_obs[i]/(high_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^2 +
            high_rs$vari12CCO2_ref[i]/(high_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^4 +
            low_rs$vari12CCO2_ref[i]/(high_rs$conc12CCO2_obs[i]-low_rs$conc12CCO2_obs[i])^4
          
          vari.g13C[i] <- high_rs$vari13CCO2_obs[i]/(high_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^2 + 
            low_rs$vari13CCO2_obs[i]/(high_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^2 +
            high_rs$vari13CCO2_ref[i]/(high_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^4 +
            low_rs$vari13CCO2_ref[i]/(high_rs$conc13CCO2_obs[i]-low_rs$conc13CCO2_obs[i])^4
          
          vari.o12C[i] <- high_rs$vari12CCO2_ref[i] + high_rs$conc12CCO2_obs[i]^2*vari.g12C[i] + gain12C[i]^2*high_rs$vari12CCO2_obs[i]
          
          vari.o13C[i] <- high_rs$vari13CCO2_ref[i] + high_rs$conc13CCO2_obs[i]^2*vari.g13C[i] + gain13C[i]^2*high_rs$vari13CCO2_obs[i]
          
        }
      } else { # MUST be medium and high points that are good.
        
        gain12C[i] <- (high_rs$conc12CCO2_ref[i] - med_rs$conc12CCO2_ref[i])/(high_rs$conc12CCO2_obs[i] - med_rs$conc12CCO2_obs[i])
        gain13C[i] <- (high_rs$conc13CCO2_ref[i] - med_rs$conc13CCO2_ref[i])/(high_rs$conc13CCO2_obs[i] - med_rs$conc13CCO2_obs[i])
        
        offset12C[i] <- high_rs$conc12CCO2_ref[i] - gain12C[i]*high_rs$conc12CCO2_obs[i]
        offset13C[i] <- high_rs$conc13CCO2_ref[i] - gain13C[i]*high_rs$conc13CCO2_obs[i]  
        
        # calculate uncertainties - standard propogation of uncertainty formula.
        vari.g12C[i] <- high_rs$vari12CCO2_obs[i]/(high_rs$conc12CCO2_obs[i]-med_rs$conc12CCO2_obs[i])^2 + 
          med_rs$vari12CCO2_obs[i]/(high_rs$conc12CCO2_obs[i]-med_rs$conc12CCO2_obs[i])^2 +
          high_rs$vari12CCO2_ref[i]/(high_rs$conc12CCO2_obs[i]-med_rs$conc12CCO2_obs[i])^4 +
          med_rs$vari12CCO2_ref[i]/(high_rs$conc12CCO2_obs[i]-med_rs$conc12CCO2_obs[i])^4
        
        vari.g13C[i] <- high_rs$vari13CCO2_obs[i]/(high_rs$conc13CCO2_obs[i]-med_rs$conc13CCO2_obs[i])^2 + 
          med_rs$vari13CCO2_obs[i]/(high_rs$conc13CCO2_obs[i]-med_rs$conc13CCO2_obs[i])^2 +
          high_rs$vari13CCO2_ref[i]/(high_rs$conc13CCO2_obs[i]-med_rs$conc13CCO2_obs[i])^4 +
          med_rs$vari13CCO2_ref[i]/(high_rs$conc13CCO2_obs[i]-med_rs$conc13CCO2_obs[i])^4
        
        vari.o12C[i] <- high_rs$vari12CCO2_ref[i] + high_rs$conc12CCO2_obs[i]^2*vari.g12C[i] + gain12C[i]^2*high_rs$vari12CCO2_obs[i]
        
        vari.o13C[i] <- high_rs$vari13CCO2_ref[i] + high_rs$conc13CCO2_obs[i]^2*vari.g13C[i] + gain13C[i]^2*high_rs$vari13CCO2_obs[i]
        
      } # if low == 1
      
    } else if (is.na(val.df$tot[i]) | val.df$tot[i] < 2) {
      
      # can't really do anything here if less than 2 valid points, 
      # set as missing, and fill w/ last known good calibration later?
      
      gain12C[i] <- gain13C[i] <- offset12C[i] <- offset13C[i] <- NA
      vari.g12C[i] <- vari.g13C[i] <- vari.o12C[i] <- vari.o13C[i] <- NA
      
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

  calVal.flag2 <- val.df$tot  
  # calVal.flag2 <- ifelse(val.df$tot > 1,
  #                        1, # set to pass if 2+ valid points.
  #                        0) # otherwise, set to fail.
  # #--------------------------------------------------------------------
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
  if (nrow(val.df) == 1 & is.na(val.df$low) & is.na(val.df$med) & is.na(val.df$high)) {
    out <- data.frame(start=NA,
                      end=NA,
                      gain12C=NA,gain13C=NA,offset12C=NA,offset13C=NA,
                      diff.12C=NA,diff.13C=NA,diff.delta=NA,
                      calVal.flag1=NA,calVal.flag2=NA,
                      vari.o12C=NA,vari.o13C=NA,vari.g13C=NA,vari.g12C=NA)
  } else {
    out <- data.frame(start=as.POSIXct(starttimes,tz="UTC",origin="1970-01-01"),
                      end=as.POSIXct(endtimes,tz="UTC",origin="1970-01-01"),
                      gain12C,gain13C,offset12C,offset13C,
                      diff.12C,diff.13C,diff.delta,
                      calVal.flag1,calVal.flag2,
                      vari.g12C,vari.g13C,vari.o12C,vari.o13C)
  }

  
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
  h5createGroup(outname,paste0('/',site,'/dp01'))
  h5createGroup(outname,paste0('/',site,'/dp01/data'))
  h5createGroup(outname,paste0('/',site,'/dp01/data/isoCo2'))
  
  fid <- H5Fopen(outname)

  # copy attributes from source file and write to output file.
  tmp <- h5readAttributes(inname,paste0('/',site))
  
  attrloc <- H5Gopen(fid,paste0('/',site))
  
  for (i in 1:length(tmp)) { # probably a more rapid way to do this in the future...lapply?
    h5writeAttribute(h5obj=attrloc,attr=tmp[[i]],name=names(tmp)[i])
  }
  
  H5Gclose(attrloc)
  
  co2.cal.outloc <- H5Gopen(fid,paste0('/',site,'/dp01/data/isoCo2'))
  
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
