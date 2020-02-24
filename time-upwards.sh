cd ../regraph
git checkout duplicates-fixed-2
raco make -v main.rkt
cd ../time-regraph
racket time-regraph.rkt
