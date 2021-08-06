#!/usr/bin/env bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/herbie/reports"

upload () {
    DIR="$1"
    B=$(git rev-parse --abbrev-ref HEAD)
    C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
    RDIR="$(date +%s):$(hostname):$B:$C"
    find "$DIR" -name "*.txt" -exec gzip -f {} \;
    find "$DIR" -name "*.json" -exec gzip -f {} \;
    rsync --recursive "$DIR" --exclude reports/ "$RHOST:$RHOSTDIR/$RDIR"
    ssh "$RHOST" chmod a+rx "$RHOSTDIR/$RDIR" -R
    if command -v nightly-results &>/dev/null; then
        nightly-results url https://herbie.uwplse.org/reports/"$RDIR"/
    fi
}

index () {
    DIR="$1"
    rsync "$RHOST:$RHOSTDIR/index.cache" index.cache
    racket infra/make-index.rkt index.cache "$DIR"
    rsync index.cache "$RHOST:$RHOSTDIR/index.cache"
    rsync --recursive \
          "index.html" "infra/index.css" "infra/regression-chart.js" "src/web/report.js" \
          "$RHOST:$RHOSTDIR/"
    ssh "$RHOST" chgrp uwplse "$RHOSTDIR/{index.html,index.css,report.js,regression-chart.js}"
    rm index.cache index.html
}

reindex () {
    DIR="$1"
    rsync --recursive --checksum --inplace --ignore-existing \
          --include 'results.json' --include 'results.json.gz' --include '*/' --exclude '*' \
          "$RHOST:$RHOSTDIR" "$DIR"
    find "$DIR" -name "results.json.gz" -exec gunzip -f {} \;
    racket infra/make-index.rkt "$DIR"
    rsync index.cache "$RHOST:$RHOSTDIR/index.cache"
    rsync --recursive \
          "index.html" "infra/index.css" "infra/regression-chart.js" "src/web/report.js" \
          "$RHOST:$RHOSTDIR/"
    ssh "$RHOST" chgrp uwplse "$RHOSTDIR/{index.html,index.css,report.js,regression-chart.js}"
    rm index.cache index.html
}

upload_reports () {
    DIR="$1"
    rsync --recursive "$DIR"/ "$RHOST:$RHOSTDIR"
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
    index "$DIR"
elif [[ $CMD = "update-index" ]]; then
    reindex "$DIR"
elif [[ $CMD = "update-reports" ]]; then
    upload_reports "$DIR"
    reindex "$DIR"
else
    help
fi

