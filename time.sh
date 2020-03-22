#!/bin/bash
set -e -x

export PATH="$PATH:/opt/racket-7.5/bin/"

run_branch () {
    branch="$1"
    dir="$2"
    shift
    shift
    git -C regraph checkout "$branch"
    git -C regraph pull
    raco make -v ./regraph/main.rkt
    mkdir -p "$dir"
    racket time-regraph.rkt "$@" "$dir" exprs/*.txt
}

time-upwards () {
    rm -rf timing-upwards
    rm -rf tables-upwards
    run_branch duplicates-fixed-2 timing-upwards/
}

time-rebuilding () {
    rm -rf timing-rebuilding
    rm -rf tables-rebuilding
    run_branch rebuilding timing-rebuilding/ --rebuild
}

raco pkg install --auto debug

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
