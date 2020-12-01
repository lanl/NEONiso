#' NEONiso: A package for calibrating NEON atmospheric isotope observations.
#' 
#' @docType package
NULL

# suppress R CMD CHECK notes on variables that are internal to this package.
globalVariables(c("CO2_obs_mean","CO2_ref_mean","c12","c13",
                  "cal_period","co2_intercept","co2_r2","co2_slope",
                  "conc12CCO2_obs", "conc12CCO2_ref", "d13C_intercept",
                  "d13C_obs_btime", "d13C_obs_etime", "d13C_obs_mean",
                  "d13C_obs_n", "d13C_obs_var", "d13C_r2", "d13C_ref_btime",
                  "d13C_ref_etime", "d13C_ref_mean", "d13C_slope",
                  "d18O_meas_btime", "d2H_meas_betime", "d2H_meas_etime",
                  "d2H_ref_btime", "d2H_ref_etime", "d18O_meas_etime",
                  "d18O_meas_n", "d18O_ref_btime", "d18O_ref_etime", 
                  "d2H_meas_btime", "day", "dom", "gain12C", "gain13C",
                  "h_intercept", "h_r2", "h_slope", "height", "level", 
                  "mean13C", "mean18O", "mean2H", "meanCo2", "meanH2o",
                  "o_intercept", "o_r2", "o_slope", "offset12C", "offset13C",
                  "r2_12C", "r2_13C", "ref13C", "ref18O", "ref2H", "refCo2",
                  "timeBgn", "timeEnd", "ucal13C", "ucal18O", "ucal2H", "ucalCo2",
                  "variCo2","vari13C","data.isoCo2.dlta13CCo2.vari","data.isoCo2.rtioMoleDryCo2.vari",
                  "valid_period_start", "verticalPosition",
                  "data.isoCo2.dlta13CCo2.mean", "data.isoCo2.dlta13CCo2.mean_cal",
                  "data.isoCo2.dlta13CCo2Refe.mean",
                  "data.isoCo2.rtioMoleDryCo2.mean", "data.isoCo2.rtioMoleDryCo2.mean_cal",
                  "data.isoCo2.rtioMoleDryCo2Refe.mean",
                  "data.isoH2o.dlta18OH2o.mean", "data.isoH2o.dlta18OH2o.mean_cal",
                  "data.isoH2o.dlta18OH2oRefe.mean",
                  "data.isoH2o.dlta2HH2o.mean", "data.isoH2o.dlta2HH2o.mean_cal",
                  "data.isoH2o.dlta2HH2oRefe.mean", "data.isoH2o.rtioMoleWetH2o.mean",
                  "standard", "std_name", "..density..")) # not sure what package ..density.. is from