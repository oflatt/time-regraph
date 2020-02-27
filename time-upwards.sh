#!/bin/bash

export PATH="$PATH:/opt/racket-7.2/bin/"

cd ./regraph
git checkout duplicates-fixed-2
git pull
raco make -v main.rkt
cd ..
racket time-regraph.rkt
