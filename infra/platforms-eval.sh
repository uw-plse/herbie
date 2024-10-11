#!/bin/bash

# exit immediately upon first error
set -e -x

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done

INFRA_DIR="$(cd -P "$(dirname "$src")" && pwd)"
BENCH_DIR="$INFRA_DIR"/../bench
THREADS=4
DEFAULT_START_SEED=1

# check arguments
if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <output_dir> <num_seeds>"
  exit 1
else
  OUTDIR="$(pwd)/$1"
  NUM_SEEDS=$2
fi

# advise user of execution plan
if [ -z "$PARALLEL_SEEDS" ]; then
  echo "Using Herbie concurrency only."
  PARALLEL_SEEDS=1
else
  # support for exporting bash environment to parallel
  echo "Using multiple concurrent Herbie runs in parallel."
  echo "Restricting to $PARALLEL_SEEDS parallel concurrent Herbie runs."
fi

# check for start seed
if [ -z "$START_SEED" ]; then
  START_SEED=$DEFAULT_START_SEED
  echo "Start seed not specified. Starting at seed $START_SEED"
else
  echo "Starting at seed $START_SEED"
fi

echo "Running platforms evaluation"
date

function run() {
  bench=$1
  key=$2
  num_runs=$3

  python3 $INFRA_DIR/platforms-eval.py \
    --key $key \
    --parallel $PARALLEL_SEEDS \
    --threads $THREADS \
    --start-seed $START_SEED \
    $bench \
    "$OUTDIR/platforms" \
    $num_runs
}

# Run configs

# Original:
# run $BENCH_DIR/hamming hamming $NUM_SEEDS
# run $BENCH_DIR/mathematics mathematics $NUM_SEEDS

run $BENCH_DIR/graphics graphics $NUM_SEEDS
run $BENCH_DIR/hamming hamming $NUM_SEEDS
run $BENCH_DIR/libraries libraries $NUM_SEEDS
run $BENCH_DIR/mathematics mathematics $NUM_SEEDS
run $BENCH_DIR/numerics numerics $NUM_SEEDS
run $BENCH_DIR/physics physics $NUM_SEEDS
run $BENCH_DIR/haskell.fpcore haskell $NUM_SEEDS
run $BENCH_DIR/regression.fpcore regression $NUM_SEEDS

python3 $INFRA_DIR/platforms/cross-plot.py "$OUTDIR/platforms/output"

echo "Finished platforms evaluation"
date

# clean up cache and build files
if [ -n "$RM_CACHE" ]; then
  echo "removing cache and drivers"
  rm -rf "$OUTDIR/platforms/herbie-2.0"
  rm -rf "$OUTDIR/platforms/cache"
  rm -rf "$OUTDIR/platforms/drivers"
fi
