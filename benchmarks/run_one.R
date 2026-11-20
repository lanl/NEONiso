# run_one.R
# Run a single workload in a fresh R process.
#
# Usage:
#   Rscript benchmarks/run_one.R <pkg_path> <workload> <method> <mode> <out_dir>
#
#   pkg_path : package source tree to load with pkgload::load_all()
#   workload : W0 | W1 | W2
#   method   : gainoffset | linreg | water
#   mode     : time -> calibrate with write_to_file = TRUE, print elapsed seconds
#              save -> also save outputs (RDS of write_to_file = FALSE result
#                      and the HDF5 file) to out_dir for compare_outputs.R
#
# set.seed(1) is called before every calibration call because the 5-fold
# cross-validation columns depend on the RNG.

args <- commandArgs(trailingOnly = TRUE)
pkg_path <- normalizePath(args[1])
workload <- args[2]
method   <- args[3]
mode     <- args[4]
out_dir  <- args[5]

here <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "workloads.R"))

suppressMessages(pkgload::load_all(pkg_path, quiet = TRUE))
files <- workload_files(workload, pkg_path)

quietly <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(suppressMessages(expr))))
  res
}

h5_out <- tempfile(fileext = ".h5")
set.seed(1)
t0 <- proc.time()[["elapsed"]]
# Before NEONiso 0.9.0, validate_output_file() only accepted 09m CO2 groups,
# so calibrate_carbon(write_to_file = TRUE) stopped at the very end on current
# (06m) NEON files, after the output file was fully written. Tolerate exactly
# that error so older versions can still be timed and their HDF5 output
# compared; any other error still fails.
tryCatch(
  quietly(run_workload(files, method, write_to_file = TRUE, outname = h5_out)),
  error = function(e) {
    if (!grepl("Output file structure has diverged", conditionMessage(e)))
      stop(e)
    cat("NOTE validate_output_file error tolerated (bug B7)\n")
  })
elapsed <- proc.time()[["elapsed"]] - t0
cat(sprintf("ELAPSED %.4f\n", elapsed))

if (mode == "save") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  tag <- paste0(workload, "_", method)
  file.copy(h5_out, file.path(out_dir, paste0(tag, ".h5")), overwrite = TRUE)
  set.seed(1)
  res <- quietly(run_workload(files, method, write_to_file = FALSE,
                              outname = tempfile()))
  saveRDS(res, file.path(out_dir, paste0(tag, ".rds")))
}
unlink(h5_out)
