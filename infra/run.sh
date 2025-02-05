#!/bin/bash

# exit immediately upon first error
if [ -z "$LOG" ]; then
  set -e
else
  set -e -x
fi

# determine where to run
if [ -z "$1" ] || [ -z "$2" ]; then
  echo "Usage: $0 <bench_dir> <output_dir>"
  echo "Optionally set LOG for logging"
  echo "Optionally set RECURSE to run each subdirectory separately"
  exit 1
else
  BENCH="$1"; shift
  OUTDIR="$1"; shift
  ARGS="$@"
fi

# on failure
function output_error {
  NAME="$2"
  DATE=`date +%s`
  COMMIT=`git rev-parse HEAD`
  BRANCH=`git rev-parse --abbrev-ref HEAD`
  HOSTNAME=`hostname`
  cat >"$1" <<EOF
{ "date": $DATE, "commit": "$COMMIT", "branch": "$BRANCH",
  "hostname": "$HOSTNAME", "seed": "$SEED", "flags": [],
  "points": -1, "iterations": -1, "bit_width": -1,
  "note": "$NAME", "crash": true, "tests": [] }
EOF
  exit 1
}

# actual runner
function run {
  bench="$1"; shift
  name="$1"; shift
  args="$@"

  if [ -z "${args}" ]; then
    echo "Running tests in '$name' with default flags"
  else
    echo "Running tests in '$name' with '$args'"
  fi

  rm -rf "$OUTDIR/$name"
  racket -y "src/main.rkt" report \
    --note "$name" \
    "$@" \
    "$bench" "$OUTDIR/$name" \
    || output_error "$OUTDIR/$name/results.json" "$name"
}

# run
mkdir -p $OUTDIR
rm -rf "$OUTDIR"/* || echo "nothing to delete"
if [ -z "$RECURSE" ]; then
  name=$(basename "$BENCH" .fpcore)
  run "$BENCH" "$name" "$@"
else
  dirs=""
  for bench in $BENCH/*; do
    name=$(basename "$bench" .fpcore)
    run "$bench" "$name" $ARGS
    if [ "$?" -eq 0 ]; then
      dirs="$dirs $name";
    fi
  done

  # merge reports
  echo "merging $dirs"
  racket -y infra/merge.rkt --name "$(basename $BENCH .fpcore)" "$OUTDIR" $dirs
fi
