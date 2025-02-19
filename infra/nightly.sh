#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
BENCHDIR="$1"; shift
REPORTDIR="$1"; shift

mkdir -p "$REPORTDIR"
rm -rf "reports"/* || echo "nothing to delete"

# run
dirs=""
for bench in "$BENCHDIR"/*; do
  name=$(basename "$bench" .fpcore)
  rm -rf "$REPORTDIR"/"$name"

  racket -y "src/main.rkt" report \
         --seed "$SEED" \
         "$@" \
         "$bench" "$REPORTDIR"/"$name"
  
  dirs="$dirs $name";
done

# merge reports
racket -y infra/merge.rkt "$REPORTDIR" $dirs

