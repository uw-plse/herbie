#!/usr/bin/env bash
set -e -x

CMD="$1"
DIR="$2"

[ -z "$DIR" ] && echo "Please pass a directory to upload"
[ ! -d "$DIR" ] && echo "Directory $DIR does not exist"

upload () {
    DIR="$1"

    B=$(git rev-parse --abbrev-ref HEAD)
    C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
    RDIR="$(date +%s):$(hostname):$B:$C"

    nightly-results download index.cache index.cache
    racket -y infra/make-index.rkt --relpath "$RDIR" index.cache "$DIR"
    nightly-results publish --name index.cache index.cache
    nightly-results publish --name index.html index.html
    nightly-results publish --name index.css infra/index.css
    nightly-results publish --name report.js src/reports/resources/report.js
    nightly-results publish --name regression-chart.js infra/regression-chart.js

    find "$DIR" -name "timeline.html" -exec gzip -f {} \;
    find "$DIR" -name "*.txt" -exec gzip -f {} \;
    find "$DIR" -name "*.json" -exec gzip -f {} \;
    nightly-results publish --name "$RDIR" "$DIR"
}

index () {
    nightly-results download index.cache index.cache
    racket -y infra/make-index.rkt index.cache
    nightly-results publish --name index.cache index.cache
    nightly-results publish --name index.html index.html
    nightly-results publish --name index.css infra/index.css
    nightly-results publish --name report.js src/reports/resources/report.js
    nightly-results publish --name regression-chart.js infra/regression-chart.js
}

help () {
    printf "USAGE: publish.sh upload <dir>\t\t\tUpload the directory <dir>\n"
    printf "       publish.sh index <dir>\t\t\t\tAdd the directory <dir> to the index page\n"
}

if [[ $CMD = "upload" ]]; then
    upload "$DIR"
elif [[ $CMD = "index" ]]; then
    index
else
    help
fi

