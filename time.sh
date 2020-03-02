#!/bin/bash

export PATH="$PATH:/opt/racket-7.5/bin/"

run_branch () {
    branch="$1"
    dir="$2"
    shift
    shift
    git -C regraph checkout "$branch"
    git -C regraph pull
    raco make -v main.rkt
    mkdir -p "$dir"
    racket time-regraph.rkt "$@" "$dir" exprs/*.txt
}

time-upwards () {
    run_branch duplicates-fixed-2 timing-upwards/
}

time-rebuilding () {
    run_branch rebuilding timing-rebuilding/ --rebuild
}

if [ ! -d regraph ] ; then
    git clone https://github.com/pavpanchekha/regraph
fi

if [[ $1 = "upwards" ]]; then
    run_branch duplicates-fixed-2 timing-upwards/
elif [[ $1 = "rebuilding" ]]; then
    run_branch rebuilding timing-rebuilding/ --rebuild
else
    "$0" upwards
    "$0" rebuilding
fi


