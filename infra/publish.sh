#!/usr/bin/env bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/herbie/reports"

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

reindex () {
    DIR="$1"
    rsync --recursive --checksum --inplace --ignore-existing \
          --include 'results.json' --include 'results.json.gz' --include '*/' --exclude '*' \
          "$RHOST:$RHOSTDIR/" "$DIR/"
    find "$DIR" -name "results.json.gz" -exec gunzip -f {} \;
    racket -y infra/make-index.rkt "$DIR"
    rsync index.cache "$RHOST:$RHOSTDIR/index.cache"
    rsync "index.html" "infra/index.css" "infra/regression-chart.js" "src/reports/resources/report.js" \
          "$RHOST:$RHOSTDIR/"
    ssh "$RHOST" chgrp uwplse "$RHOSTDIR/{index.html,index.css,report.js,regression-chart.js}"
    rm index.html
}

upload_reports () {
    DIR="$1"
    rsync --recursive "$DIR/" "$RHOST:$RHOSTDIR/"
}

help () {
    printf "USAGE: publish.sh upload <dir>\t\t\tUpload the directory <dir>\n"
    printf "       publish.sh index <dir>\t\t\t\tAdd the directory <dir> to the index page\n"
}

CMD="$1"
DIR="$2"

check_dir () {
    if [[ -z $DIR ]]; then
        echo "Please pass a directory to upload"
        echo
        help
        exit 1
    elif [[ ! -d $DIR ]]; then
        echo "Directory $DIR does not exist"
        exit 2
    else
        return 0
    fi
}

if [[ $CMD = "upload" ]]; then
    check_dir
    upload "$DIR"
elif [[ $CMD = "index" ]]; then
    index
elif [[ $CMD = "update-index" ]]; then
    check_dir
    reindex "$DIR"
elif [[ $CMD = "update-reports" ]]; then
    check_dir
    upload_reports "$DIR"
    reindex "$DIR"
else
    help
fi

