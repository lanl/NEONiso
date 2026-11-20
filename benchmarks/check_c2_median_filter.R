# check_c2_median_filter.R
# Equivalence check for rolling_median() / filter_median_brock86() (C2)
# against the original zoo::rollapply(median) implementation.
#
# Usage: Rscript benchmarks/check_c2_median_filter.R [pkg_path]

args <- commandArgs(trailingOnly = TRUE)
pkg_path <- if (length(args)) args[1] else "."
suppressMessages(pkgload::load_all(pkg_path, quiet = TRUE))

orig_roll <- function(x, width) {
  zoo::rollapply(x, width, median, na.rm = TRUE, fill = NA)
}
orig_filter <- function(data, width = 7, threshold = 5) {
  filt <- zoo::rollapply(data, width, median, na.rm = TRUE, fill = NA)
  spikes <- abs(data - filt) > threshold
  data[spikes == TRUE] <- NA
  data
}

set.seed(20261119)
gen <- function() {
  n <- sample(c(0:20, 50, 500, 5000), 1)
  x <- switch(sample(5, 1),
              rnorm(n, -8, 3),                       # continuous
              round(rnorm(n, -8, 3), 1),             # many ties
              sample(c(-1, 0, 1, 2), n, TRUE),       # heavy ties
              rnorm(n) * 1e6,                        # large values
              runif(n, -0.5, 0.5))                   # small values
  if (n > 0) {
    p_na <- sample(c(0, 0.05, 0.3, 0.5, 0.9, 1), 1)
    x[runif(n) < p_na] <- NA
    if (runif(1) < 0.2) x[sample(n, max(1, n %/% 20))] <- NaN
    if (runif(1) < 0.2) x[sample(n, 1)] <- Inf
    if (runif(1) < 0.2) x[sample(n, 1)] <- -Inf
    if (runif(1) < 0.3) x[sample(n, max(1, n %/% 50))] <- 40   # spikes
  }
  x
}

n_cases <- 20000
bad <- 0
for (k in seq_len(n_cases)) {
  x <- gen()
  w <- sample(c(7, 7, 7, 1, 3, 5, 9), 1)
  if (!identical(orig_roll(x, w), rolling_median(x, w)) ||
        !identical(orig_filter(x, w), filter_median_brock86(x, w))) {
    bad <- bad + 1
    if (bad <= 5) { cat("MISMATCH width", w, "\n"); print(x) }
  }
}
cat(sprintf("random vectors: %d checked, %d mismatches\n", n_cases, bad))

# inputs that must take the zoo fallback
fallback <- list(
  integer   = sample(-5:5, 100, TRUE),
  logical   = rep(NA, 50),
  named     = setNames(rnorm(20), letters[1:20]),
  even_w    = list(rnorm(100), 6),
  short     = rnorm(4)
)
for (nm in names(fallback)) {
  v <- fallback[[nm]]
  w <- 7
  if (is.list(v)) { w <- v[[2]]; v <- v[[1]] }
  same <- identical(orig_roll(v, w), rolling_median(v, w)) &&
    identical(orig_filter(v, w), filter_median_brock86(v, w))
  if (!same) bad <- bad + 1
  cat(sprintf("fallback %-8s %s\n", nm, if (same) "IDENTICAL" else "DIFFERS"))
}

x <- rnorm(50000, -8, 2); x[sample(50000, 2500)] <- NA
print(bench::mark(original = orig_filter(x), current = filter_median_brock86(x),
                  iterations = 5)[, c("expression", "median", "mem_alloc")])
if (bad > 0) quit(status = 1)
