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
quietly(run_workload(files, method, write_to_file = TRUE, outname = h5_out))
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
