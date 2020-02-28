#!/bin/bash

export PATH="$PATH:/opt/racket-7.2/bin/"

cd ./regraph
git checkout duplicates-fixed-2
git pull
raco make -v main.rkt
cd ..
make -p timing-upwards
racket time-regraph.rkt timing-upwards
