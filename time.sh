#!/bin/bash
set -e -x

export PATH="$PATH:/opt/racket-7.5/bin/"

run_branch () {
    dir="$1"
    shift
    git -C regraph checkout master
    git -C regraph pull
    raco make -v ./regraph/main.rkt
    mkdir -p "$dir"
    racket time-regraph.rkt "$@" "$dir" exprs/*.txt
}

time-upwards () {
    rm -rf timing-upwards
    rm -rf tables-upwards
    run_branch timing-upwards/
}

time-rebuilding () {
    rm -rf timing-rebuilding
    rm -rf tables-rebuilding
    run_branch timing-rebuilding/ --rebuild
}

mkdir -p "limits"

if [ ! -d regraph ] ; then
    git clone https://github.com/pavpanchekha/regraph
fi

if [[ $1 = "upwards" ]]; then
    time-upwards
elif [[ $1 = "rebuilding" ]]; then
    time-rebuilding
else
    time-upwards
    time-rebuilding
fi

racket process-data.rkt
