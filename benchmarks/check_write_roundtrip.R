# check_write_roundtrip.R
# End-to-end check of the write_to_file route: every data frame that
# calibrate_*(write_to_file = FALSE) returns must be in the file written by
# write_to_file = TRUE (same seed), with the same columns and values, and the
# file must hold nothing else under dp01/data/iso*.
#
# Usage: Rscript benchmarks/check_write_roundtrip.R <pkg_path> <workload> <method>
args <- commandArgs(trailingOnly = TRUE)
pkg <- args[1]; wl <- args[2]; method <- args[3]
pkgload::load_all(pkg, quiet = TRUE)
here <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "workloads.R"))
files <- workload_files(wl, pkg)
site <- "ONAQ"
out <- tempfile(fileext = ".h5")
run <- function(write) {
  set.seed(1)
  suppressWarnings(if (method == "water") {
    calibrate_water(files, out, site, write_to_file = write)
  } else {
    calibrate_carbon(files, out, site, method = method, write_to_file = write)
  })
}
invisible(capture.output(mem <- run(FALSE), run(TRUE)))
iso <- if (method == "water") "isoH2o" else "isoCo2"
amb <- if (method == "water") mem$wiso_subset_cal else mem$ciso_subset_cal
h5 <- hdf5r::H5File$new(out, mode = "r")
base <- paste0(site, "/dp01/data/", iso)
problems <- character()
in_file <- setdiff(h5[[base]]$ls()$name, "calData")
if (!setequal(in_file, names(amb))) {
  problems <- c(problems, sprintf("groups: file {%s} vs returned {%s}",
    paste(sort(in_file), collapse = ","), paste(sort(names(amb)), collapse = ",")))
}
n_checked <- 0
for (g in intersect(in_file, names(amb))) {
  vars_file <- h5[[paste0(base, "/", g)]]$ls()$name
  if (!setequal(vars_file, names(amb[[g]]))) {
    problems <- c(problems, sprintf("%s: variables differ", g))
  }
  for (v in intersect(vars_file, names(amb[[g]]))) {
    got <- h5[[paste0(base, "/", g, "/", v)]]$read()
    want <- as.data.frame(amb[[g]][[v]])
    if (!setequal(names(got), names(want))) {
      problems <- c(problems, sprintf("%s/%s: columns differ", g, v)); next
    }
    for (col in names(want)) {
      a <- want[[col]]; b <- got[[col]]
      same <- length(a) == length(b) &&
        all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
      if (!isTRUE(same)) problems <- c(problems, sprintf("%s/%s$%s differs", g, v, col))
    }
    n_checked <- n_checked + 1
  }
}
cal <- h5[[paste0(base, "/calData")]]$ls()$name
if (length(cal) != 1) {
  problems <- c(problems, "calData should hold exactly one dataset")
} else {
  got <- h5[[paste0(base, "/calData/", cal)]]$read()
  want <- as.data.frame(mem$cal_df)
  for (col in names(want)) {
    a <- want[[col]]; b <- got[[col]]
    same <- length(a) == length(b) &&
      all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
    if (!isTRUE(same)) problems <- c(problems, sprintf("calData$%s differs", col))
  }
}
cat(sprintf("%s %s: %d datasets checked; calData: {%s}\n", wl, method,
            n_checked, paste(cal, collapse = ",")))
h5$close_all()
if (length(problems)) { cat(problems, sep = "\n"); quit(status = 1) }
cat("ROUNDTRIP OK\n")
