#!/bin/bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/regraph"

upload () {
    DIR="$(date +%s)"
    mkdir -p report
    mv search-time.png total-time.png report/
    mv tables-upwards/averages.txt report/upwards.txt
    mv tables-rebuilding/averages.txt report/rebuilding.txt
    cp index.css report/
    racket index.rkt report/upwards.txt report/rebuilding.txt report/index.html
    rsync --perms --chmod 755 --recursive report/ "$RHOST:$RHOSTDIR/$DIR"
}

upload
