# workloads.R
# Reference workloads for NEONiso performance work.
#
#   W0: bundled ONAQ 2019-05 test fixture (inst/extdata), small/smoke test.
#   W1: one full real monthly DP4.00200.001 basic file, ONAQ 2019-05.
#   W2: twelve real monthly files, ONAQ 2019-01 ... 2019-12, site-level run.
#   (W3 = peak memory of W2; measured by bench.sh, not a separate input set.)
#
# Real files are expected (unzipped, as .h5) somewhere under
# $NEONISO_BENCH_DATA (default: ~/NEONiso-benchmark-data).

bench_data_dir <- function() {
  Sys.getenv("NEONISO_BENCH_DATA",
             unset = path.expand("~/NEONiso-benchmark-data"))
}

workload_files <- function(id, pkg_path) {
  if (id == "W0") {
    return(file.path(pkg_path, "inst", "extdata",
                     "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5"))
  }
  all_h5 <- sort(list.files(bench_data_dir(),
                            pattern = "ONAQ.*\\.h5$",
                            recursive = TRUE, full.names = TRUE))
  if (id == "W1") {
    f <- all_h5[grepl("2019-05", all_h5)]
  } else if (id == "W2") {
    f <- all_h5[grepl("2019-(0[1-9]|1[0-2])", all_h5)]
  } else {
    stop("unknown workload: ", id)
  }
  if (length(f) == 0) stop("no data files found for ", id, " in ",
                           bench_data_dir())
  f
}

# Run one workload/method. Returns the calibrate_* result
# (write_to_file = FALSE) or NULL (write_to_file = TRUE).
run_workload <- function(files, method, write_to_file, outname) {
  if (method == "water") {
    calibrate_water(files, outname, "ONAQ", write_to_file = write_to_file)
  } else {
    calibrate_carbon(files, outname, "ONAQ", method = method,
                     write_to_file = write_to_file)
  }
}
