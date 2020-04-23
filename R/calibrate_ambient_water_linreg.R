#' calibrate_ambient_water_isotopes
#' 
#' @author Rich Fiorella \email{rich.fiorella@@utah.edu}
#'
#' @param amb.data.list 
#' @param caldf 
#' @param outname 
#' @param site 
#' @param file 
#' @param force.to.end 
#' @param force.to.beginning 
#'
#' @return
#' @export
#'
#' @examples
calibrate_ambient_water_linreg <- function(amb.data.list,caldf,outname,site,file,force.to.end=TRUE,force.to.beginning=TRUE) {
  
  # required libraries
  require(rhdf5)
  
  # print status.
  print("Processing water ambient data...")
  
  # In contrast to carbon calibration - need to get both 18O and 2H separately...
  oxydf <- amb.data.list$dlta18OH2o
  hyddf <- amb.data.list$dlta2HH2o
  
  # 190103 - rpf. need separate processing for oxygen and hydrogen in case there are a different 
  # number of rows between oxygen and hydrogen. not sure yet why this would arise, but it does 
  # appear in the NEON archive (OSBS).
  #-------------------------------------------------------
  # oxygen.
  #-------------------------------------------------------
  # ensure that time variables are in POSIXct. (note: these should be the same for 18O and 2H?)
  amb.start.times <- as.POSIXct(oxydf$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
  amb.end.times <- as.POSIXct(oxydf$timeEnd,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
  
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
  oxydf$mean_cal <- oxydf$mean
  oxydf$max_cal  <- oxydf$max
  oxydf$min_cal  <- oxydf$min 
  
  for (i in 1:length(var.inds.in.calperiod)) {
    oxydf$mean_cal[var.inds.in.calperiod[[i]]] <- oxydf$mean[var.inds.in.calperiod[[i]]]*caldf$o.slope[i] + caldf$o.intercept[i]
    oxydf$min_cal[var.inds.in.calperiod[[i]]] <- oxydf$min[var.inds.in.calperiod[[i]]]*caldf$o.slope[i] + caldf$o.intercept[i]
    oxydf$max_cal[var.inds.in.calperiod[[i]]] <- oxydf$max[var.inds.in.calperiod[[i]]]*caldf$o.slope[i] + caldf$o.intercept[i]
    
  }
  
  # replace ambdf in amb.data.list
  amb.data.list$dlta18OH2o <- oxydf
  
  #-------------------------------------------------------
  # hydrogen.
  #-------------------------------------------------------
  rm(amb.start.times,oxydf,amb.end.times,i,var.periods.in.calperiod)
  
  # ensure that time variables are in POSIXct. (note: these should be the same for 18O and 2H?)
  amb.start.times <- as.POSIXct(hyddf$timeBgn,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
  amb.end.times <- as.POSIXct(hyddf$timeEnd,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
  
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
  
  hyddf$mean_cal <- hyddf$mean
  hyddf$max_cal  <- hyddf$max
  hyddf$min_cal  <- hyddf$min 
  
  for (i in 1:length(var.inds.in.calperiod)) {
    hyddf$mean_cal[var.inds.in.calperiod[[i]]] <- hyddf$mean[var.inds.in.calperiod[[i]]]*caldf$h.slope[i] + caldf$h.intercept[i]
    hyddf$min_cal[var.inds.in.calperiod[[i]]] <- hyddf$min[var.inds.in.calperiod[[i]]]*caldf$h.slope[i] + caldf$h.intercept[i]
    hyddf$max_cal[var.inds.in.calperiod[[i]]] <- hyddf$max[var.inds.in.calperiod[[i]]]*caldf$h.slope[i] + caldf$h.intercept[i]
  
  }
  
  # replace ambdf in amb.data.list
  amb.data.list$dlta2HH2o <- hyddf

  #-----------------------------------------------------------
  # write out dataset to HDF5 file.
  fid <- H5Fopen(file)
  
  print(outname)
  h2o.data.outloc <- H5Gcreate(fid,paste0('/',site,'/dp01/data/isoH2o/',outname))
  
  # loop through each of the variables in list amb.data.list and write out as a dataframe.
  lapply(names(amb.data.list),function(x) {
    h5writeDataset.data.frame(obj=amb.data.list[[x]],
                              h5loc=h2o.data.outloc,
                              name=x,
                              DataFrameAsCompound = TRUE)})
  
  # close all open handles.
  h5closeAll()
}