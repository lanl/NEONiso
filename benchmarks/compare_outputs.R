# compare_outputs.R
# Bit-for-bit comparison of saved workload outputs (from run_one.R "save").
#
# Usage:
#   Rscript benchmarks/compare_outputs.R <baseline_dir> <candidate_dir>
#
# For every <tag>.rds in baseline_dir: identical() on the R result object.
# For every <tag>.h5: h5diff with no tolerance (exact), if h5diff is on PATH.
# Exits non-zero if anything differs.

args <- commandArgs(trailingOnly = TRUE)
base_dir <- args[1]
cand_dir <- args[2]
ok <- TRUE

for (f in list.files(base_dir, pattern = "\\.rds$")) {
  b <- readRDS(file.path(base_dir, f))
  cf <- file.path(cand_dir, f)
  if (!file.exists(cf)) { cat("MISSING ", f, "\n"); ok <- FALSE; next }
  c <- readRDS(cf)
  same <- identical(b, c)
  cat(sprintf("%-8s %s\n", if (same) "IDENTICAL" else "DIFFERS", f))
  if (!same) {
    ok <- FALSE
    print(all.equal(b, c, tolerance = 0))
  }
}

h5diff <- Sys.which("h5diff")
if (nzchar(h5diff)) {
  for (f in list.files(base_dir, pattern = "\\.h5$")) {
    cf <- file.path(cand_dir, f)
    if (!file.exists(cf)) { cat("MISSING ", f, "\n"); ok <- FALSE; next }
    out <- suppressWarnings(system2(h5diff, c(file.path(base_dir, f), cf),
                                    stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    same <- is.null(status) || status == 0
    cat(sprintf("%-8s %s (h5diff)\n", if (same) "IDENTICAL" else "DIFFERS", f))
    if (!same) { ok <- FALSE; cat(head(out, 20), sep = "\n") }
  }
} else {
  cat("h5diff not found; HDF5 files not compared\n")
}

if (!ok) quit(status = 1)
cat("ALL IDENTICAL\n")
