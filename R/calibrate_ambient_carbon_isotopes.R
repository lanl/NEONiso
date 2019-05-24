#' calibrate_ambient_carbon_isotopes
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param amb.data.list 
#' @param caldf 
#' @param outname 
#' @param site 
#' @param file 
#' @param method 
#' @param force.to.end 
#' @param force.to.beginning 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
calibrate_ambient_carbon_isotopes <- function(amb.data.list,caldf,outname,site,file,method,force.to.end=TRUE,force.to.beginning=TRUE) {
  
  if (method == 1) {
    # Method 1 refers to the Bowling et al. 2003 calibratino technique.
    # should be able to get a calGainsOffsets object from the H5 file.
    
    # print status.
    print("Processing carbon ambient data...")
    print("Using the Bowling et al 2003 approach...")
    
    # only working on the d13C of the amb.data.list, so extract just this...
    amb.delta <- amb.data.list$dlta13CCo2
    amb.12CO2 <- amb.data.list$rtioMoleDry12CCo2
    amb.13CO2 <- amb.data.list$rtioMoleDry13CCo2
    
    # ensure that time variables are in POSIXct.
    amb.start.times <- as.POSIXct(amb.delta$timeBgn,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    amb.end.times <- as.POSIXct(amb.delta$timeEnd,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    
    # determine which cal period each ambient data belongs to.
    var.inds.in.calperiod <- list()
    
    for (i in 1:nrow(caldf)) {
      int <- interval(caldf$start[i],caldf$end[i])
      var.inds.in.calperiod[[i]] <- which(amb.end.times %within% int)
    }
    
    # calibrate data at this height.
    #-------------------------------------
    # extract 12CO2 and 13CO2 concentrations from the ambient data
    
    # set parameters.
    f <- 0.000474 # fraction of CO2 that is not 12CO2 or 13CO2
    R_vpdb <- 0.0111797 # 13C/12C ratio in VPDB
    
    mean12C <- max12C <- min12C <- amb.delta$mean # create placeholders for 12CO2 vectors
    mean13C <- max13C <- min13C <- amb.delta$mean # create placeholders for 13CO2 vectors
    amb.delta$mean_cal <- amb.delta$max_cal <- amb.delta$min_cal <- amb.delta$RPF.qflag <- amb.delta$mean # placeholders for calibrated delta vals.
    
    for (i in 1:length(var.inds.in.calperiod)) {
      # calculate calibrated 12CO2 concentrations
      mean12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i]*amb.12CO2$mean[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
      min12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i]*amb.12CO2$min[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
      max12C[var.inds.in.calperiod[[i]]] <- caldf$gain12C[i]*amb.12CO2$max[var.inds.in.calperiod[[i]]] + caldf$offset12C[i]
      
      # calculate calibrated 13CO2 concentrations
      mean13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i]*amb.13CO2$mean[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]
      min13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i]*amb.13CO2$min[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]
      max13C[var.inds.in.calperiod[[i]]] <- caldf$gain13C[i]*amb.13CO2$max[var.inds.in.calperiod[[i]]] + caldf$offset13C[i]
      
      # copy over quality flag to indicate where the calibration seems to be good.
      amb.delta$RPF.qflag[var.inds.in.calperiod[[i]]] <- caldf$RPF.calVal.flag[var.inds.in.calperiod[[i]]]
    }
    
    # output calibrated delta values.
    amb.delta$mean_cal <- 1000*(mean13C/mean12C/R_vpdb - 1)
    amb.delta$min_cal <- 1000*(min13C/min12C/R_vpdb - 1)
    amb.delta$max_cal <- 1000*(max13C/max12C/R_vpdb - 1)
    
    # replace ambdf in amb.data.list, return amb.data.list
    amb.data.list$dlta13CCo2 <- amb.delta
    
    # write out dataset to HDF5 file.
    fid <- H5Fopen(file)
    
    print(outname)
    co2.data.outloc <- H5Gcreate(fid,paste0('/',site,'/dp01iso/data/isoCo2/',outname))
    
    # loop through each of the variables in list amb.data.list and write out as a dataframe.
    lapply(names(amb.data.list),function(x) {
      h5writeDataset.data.frame(obj=amb.data.list[[x]],
                                h5loc=co2.data.outloc,
                                name=x,
                                DataFrameAsCompound = TRUE)})
    
    # close all open handles.
    h5closeAll()
    
  } else if (method == 2) {
    # print status.
    print("Processing carbon ambient data...")
    
    # only working on the d13C of the amb.data.list, so extract just this...
    ambdf <- amb.data.list$dlta13CCo2
    
    # ensure that time variables are in POSIXct.
    amb.start.times <- as.POSIXct(ambdf$timeBgn,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    amb.end.times <- as.POSIXct(ambdf$timeEnd,format="%Y-%m-%dT%H:%M:%S.%OSZ",tz="UTC")
    
    # if force.to.end and/or force.to.beginning are true, match out$start[1] to min(amb time)
    # and/or out$end[nrow] to max(amb time)
    
    if (force.to.end == TRUE) {
      caldf$end[nrow(caldf)] <- amb.end.times[length(amb.end.times)]
    }
    if (force.to.beginning == TRUE) {
      caldf$start[1] <- amb.start.times[1]
    }
    
    # determine which cal period each ambient data belongs to.
    var.inds.in.calperiod <- list()
    
    for (i in 1:nrow(caldf)) {
      int <- interval(caldf$start[i],caldf$end[i])
      var.inds.in.calperiod[[i]] <- which(amb.end.times %within% int)
    }
    
    # calibrate data at this height.
    ambdf$mean_cal <- ambdf$mean
    ambdf$max_cal  <- ambdf$max
    ambdf$min_cal  <- ambdf$min
    for (i in 1:length(var.inds.in.calperiod)) {
      ambdf$mean_cal[var.inds.in.calperiod[[i]]] <- ambdf$mean[var.inds.in.calperiod[[i]]]*caldf$slope[i] + caldf$intercept[i]
      ambdf$min_cal[var.inds.in.calperiod[[i]]] <- ambdf$min[var.inds.in.calperiod[[i]]]*caldf$slope[i] + caldf$intercept[i]
      ambdf$max_cal[var.inds.in.calperiod[[i]]] <- ambdf$max[var.inds.in.calperiod[[i]]]*caldf$slope[i] + caldf$intercept[i]
    }
    
    # replace ambdf in amb.data.list, return amb.data.list
    amb.data.list$dlta13CCo2 <- ambdf
    
    # write out dataset to HDF5 file.
    fid <- H5Fopen(file)
    
    print(outname)
    co2.data.outloc <- H5Gcreate(fid,paste0('/',site,'/dp01iso/data/isoCo2/',outname))
    
    # loop through each of the variables in list amb.data.list and write out as a dataframe.
    lapply(names(amb.data.list),function(x) {
      h5writeDataset.data.frame(obj=amb.data.list[[x]],
                                h5loc=co2.data.outloc,
                                name=x,
                                DataFrameAsCompound = TRUE)})
    
    # close all open handles.
    h5closeAll()
  }
  
}
