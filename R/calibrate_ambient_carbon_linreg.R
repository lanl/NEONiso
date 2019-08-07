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
calibrate_ambient_carbon_linreg <- function(amb.data.list,caldf,outname,site,file,force.to.end=TRUE,force.to.beginning=TRUE) {

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