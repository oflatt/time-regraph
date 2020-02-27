#!/bin/bash

if [ ! -d regraph ] ; then
    git clone https://github.com/pavpanchekha/regraph
fi

bash ./time-upwards.sh
bash ./time-rebuilding.sh


