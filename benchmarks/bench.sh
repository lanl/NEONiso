#!/usr/bin/env bash
# bench.sh - time workloads in fresh R processes and record peak memory.
#
# Usage:
#   benchmarks/bench.sh <pkg_path> <label> <n_runs> <workload> [methods...]
#
# Example:
#   benchmarks/bench.sh . after_c1 5 W0 gainoffset linreg water
#
# Appends one row per run to benchmarks/results/timings.csv:
#   label,workload,method,run,elapsed_s,max_rss_mb
# elapsed_s is the in-process time of the calibrate_* call only (excludes R
# startup and package loading). max_rss_mb is the peak resident set size of
# the whole Rscript process (macOS `/usr/bin/time -l`).

set -euo pipefail
pkg=$1; label=$2; n=$3; wl=$4; shift 4
methods=("${@:-gainoffset linreg water}")
here="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$here/results"
csv="$here/results/timings.csv"
[ -f "$csv" ] || echo "label,workload,method,run,elapsed_s,max_rss_mb" > "$csv"

for m in ${methods[@]}; do
  for i in $(seq 1 "$n"); do
    log=$(mktemp)
    /usr/bin/time -l Rscript "$here/run_one.R" "$pkg" "$wl" "$m" time /dev/null \
      > "$log" 2>&1
    el=$(grep '^ELAPSED' "$log" | awk '{print $2}')
    rss=$(grep 'maximum resident set size' "$log" | awk '{printf "%.1f", $1/1048576}')
    echo "$label,$wl,$m,$i,$el,$rss" | tee -a "$csv"
    rm -f "$log"
  done
done
