#!/bin/bash

# exit immediately upon first error
set -e -x

CORES=4 # Raising this doesn't seem to speed up nightlies
SEED=$(date "+%Y%j")

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done

INFRA_DIR="$(cd -P "$(dirname "$src")" && pwd)"
BENCH_DIR="$INFRA_DIR"/../bench

# check arguments
if [ -z "$1" ]; then
  echo "Usage: $0 <output_dir>"
  exit 1
else
  OUT_DIR="$1"; shift
  FLAGS="$@"
fi

# run
RECURSE=1 LOG=1 \
  bash "$INFRA_DIR"/run.sh \
    "$BENCH_DIR" "$OUT_DIR/2/" \
    --profile \
    --seed "$SEED" \
    --threads "$CORES" \
    --platform "hardware-accelerators"
    $FLAGS

RECURSE=1 LOG=1 \
  bash "$INFRA_DIR"/run.sh \
    "$BENCH_DIR" "$OUT_DIR/1/" \
    --profile \
    --seed "$SEED" \
    --threads "$CORES" \
    --platform "arith"
    $FLAGS

# upload
if [ "$?" -eq 0 ]; then
  bash $INFRA_DIR/publish.sh upload "$OUT_DIR"
fi

