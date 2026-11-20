# summarize.R
# Summarize benchmarks/results/timings.csv: median and IQR of elapsed time,
# median peak RSS, and change vs a reference label, per workload/method.
#
# Usage:
#   Rscript benchmarks/summarize.R [reference_label] [label1,label2,...]

args <- commandArgs(trailingOnly = TRUE)
ref <- if (length(args) >= 1) args[1] else "baseline"
here <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE))))
d <- utils::read.csv(file.path(here, "results", "timings.csv"))
labels <- if (length(args) >= 2) strsplit(args[2], ",")[[1]] else unique(d$label)
d <- d[d$label %in% labels, ]

agg <- do.call(rbind, lapply(split(d, list(d$label, d$workload, d$method),
                                   drop = TRUE), function(g) {
  q <- stats::quantile(g$elapsed_s, c(0.25, 0.5, 0.75), names = FALSE)
  data.frame(label = g$label[1], workload = g$workload[1],
             method = g$method[1], n = nrow(g),
             median_s = q[2], iqr_s = q[3] - q[1],
             rss_mb = stats::median(g$max_rss_mb))
}))

base <- agg[agg$label == ref, c("workload", "method", "median_s", "rss_mb")]
names(base)[3:4] <- c("ref_s", "ref_rss")
agg <- merge(agg, base, by = c("workload", "method"), all.x = TRUE)
agg$time_change <- sprintf("%+.1f%%", 100 * (agg$median_s / agg$ref_s - 1))
agg$rss_change <- sprintf("%+.1f%%", 100 * (agg$rss_mb / agg$ref_rss - 1))
agg$label <- factor(agg$label, levels = labels)
agg <- agg[order(agg$workload, agg$method, agg$label), ]
agg$median_s <- round(agg$median_s, 3)
agg$iqr_s <- round(agg$iqr_s, 3)
print(agg[, c("workload", "method", "label", "n", "median_s", "iqr_s",
              "time_change", "rss_mb", "rss_change")], row.names = FALSE)
