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

# Timings on battery are unreliable (power throttling, and macOS maintenance /
# lid-closed sleeps land inside runs). Refuse unless explicitly allowed.
if command -v pmset >/dev/null && ! pmset -g batt | grep -q "AC Power" \
     && [ "${BENCH_ALLOW_BATTERY:-0}" != "1" ]; then
  echo "bench.sh: on battery power; plug in (or set BENCH_ALLOW_BATTERY=1)" >&2
  exit 1
fi
# keep the machine awake while a run is in progress (macOS)
keepawake=""
command -v caffeinate >/dev/null && keepawake="caffeinate -i -s"
methods=("${@:-gainoffset linreg water}")
here="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$here/results"
csv="$here/results/timings.csv"
[ -f "$csv" ] || echo "label,workload,method,run,elapsed_s,max_rss_mb" > "$csv"

for m in ${methods[@]}; do
  for i in $(seq 1 "$n"); do
    log=$(mktemp)
    $keepawake /usr/bin/time -l Rscript "$here/run_one.R" "$pkg" "$wl" "$m" time /dev/null \
      > "$log" 2>&1
    el=$(grep '^ELAPSED' "$log" | awk '{print $2}')
    rss=$(grep 'maximum resident set size' "$log" | awk '{printf "%.1f", $1/1048576}')
    echo "$label,$wl,$m,$i,$el,$rss" | tee -a "$csv"
    rm -f "$log"
  done
done
