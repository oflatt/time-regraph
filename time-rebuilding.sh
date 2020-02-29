#!/bin/bash

export PATH="$PATH:/opt/racket-7.2/bin/"

cd ./regraph
git checkout rebuilding
git pull
raco make -v main.rkt
cd ..
mkdir timing-rebuilding
make -p timing-rebuilding
racket time-regraph.rkt --rebuild timing-rebuilding
