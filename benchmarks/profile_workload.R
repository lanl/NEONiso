# profile_workload.R
# Rprof one workload/method and print the share of time in package stages.
#
# Usage:
#   Rscript benchmarks/profile_workload.R <pkg_path> <workload> <method>

args <- commandArgs(trailingOnly = TRUE)
pkg_path <- normalizePath(args[1])
workload <- args[2]
method   <- args[3]

here <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "workloads.R"))
suppressMessages(pkgload::load_all(pkg_path, quiet = TRUE))
files <- workload_files(workload, pkg_path)

# load the HDF5 backend before profiling (one-time cost)
invisible(.hdf5_backend())

pf <- tempfile(fileext = ".Rprof")
set.seed(1)
Rprof(pf, interval = 0.01)
invisible(utils::capture.output(tryCatch(
  suppressWarnings(suppressMessages(
    run_workload(files, method, TRUE, tempfile(fileext = ".h5")))),
  error = function(e) NULL)))
Rprof(NULL)

s <- summaryRprof(pf)
tot <- s$sampling.time
stages <- c("neonUtilities::stackEddy", "convert_POSIXct_to_NEONhdf5_time",
            "restructure_variables", "ingest_data",
            "fit_carbon_regression", "fit_water_regression",
            "estimate_calibration_error", "loocv",
            "calibrate_ambient_carbon_gainoffset",
            "calibrate_ambient_carbon_linreg",
            "calibrate_ambient_water_linreg", "filter_median_brock86",
            "calperiod_indices",
            "write_carbon_ambient_data", "write_water_ambient_data",
            "write_carbon_calibration_data", "write_water_calibration_data",
            "setup_output_file", "validate_output_file",
            "correct_carbon_ref_output", "correct_carbon_ref_cval",
            "extract_carbon_cal_data", "extract_water_calibration_data",
            "select_daily_reference_data")
bt <- s$by.total
rn <- gsub('"', "", rownames(bt))
keep <- rn %in% stages
out <- data.frame(stage = rn[keep], seconds = bt$total.time[keep],
                  pct = round(100 * bt$total.time[keep] / tot, 1))
cat(sprintf("== %s %s: %.2f s sampled\n", workload, method, tot))
print(out[order(-out$seconds), ], row.names = FALSE)
cat("-- top self time --\n")
print(head(s$by.self[, c("self.time", "self.pct")], 12))
