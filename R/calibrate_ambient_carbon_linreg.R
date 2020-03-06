#' calibrate_ambient_carbon_isotopes
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
#' @param r2.thres 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @importFrom magrittr %>%
#' 
calibrate_ambient_carbon_linreg <- function(amb.data.list,caldf,outname,site,file,force.to.end=TRUE,force.to.beginning=TRUE,r2.thres=0.95) {

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
      int <- lubridate::interval(caldf$start[i],caldf$end[i])
      var.inds.in.calperiod[[i]] <- which(amb.end.times %within% int)
      
      # check to see if calibration point is "valid" - 
      # at present - "valid" means r2 > r2.thres.
      # rpf - 190809.
      
      if (!is.na(caldf$r2[i]) & caldf$r2[i] < r2.thres) {
        if (i > 1) {
          caldf$slope[i] <- caldf$slope[i-1]
          caldf$intercept[i] <- caldf$intercept[i-1]
          caldf$r2[i] <- caldf$r2[i-1]
        } else {
          first.good.val <- min(which(caldf$r2 > r2.thres))
          caldf$slope[i] <- caldf$slope[first.good.val]
          caldf$intercept[i] <- caldf$intercept[first.good.val]
          caldf$r2[i] <- caldf$r2[first.good.val]
        }
      }
    }
    
    for (i in 1:length(var.inds.in.calperiod)) {
      ambdf$mean[var.inds.in.calperiod[[i]]] <- round(ambdf$mean[var.inds.in.calperiod[[i]]]*caldf$slope[i] + caldf$intercept[i], digits = 2)
      ambdf$min[var.inds.in.calperiod[[i]]] <- round(ambdf$min[var.inds.in.calperiod[[i]]]*caldf$slope[i] + caldf$intercept[i], digits = 2)
      ambdf$max[var.inds.in.calperiod[[i]]] <- round(ambdf$max[var.inds.in.calperiod[[i]]]*caldf$slope[i] + caldf$intercept[i], digits = 2)
    }
    
    # round variance down to 2 digits
    ambdf$vari <- round(ambdf$vari, digits = 2)
    
    # replace ambdf in amb.data.list, return amb.data.list
    amb.data.list$dlta13CCo2 <- ambdf
    
    # write out dataset to HDF5 file.
    fid <- rhdf5::H5Fopen(file)
    
    co2.data.outloc <- rhdf5::H5Gcreate(fid,paste0('/',site,'/dp01/data/isoCo2/',outname))
    
    # loop through each of the variables in list amb.data.list and write out as a dataframe.
    lapply(names(amb.data.list),function(x) {
      rhdf5::h5writeDataset.data.frame(obj=amb.data.list[[x]],
                                h5loc=co2.data.outloc,
                                name=x,
                                DataFrameAsCompound = TRUE)})
    
    # close all open handles.
    rhdf5::h5closeAll()
}
