

cd ../regraph
git checkout rebuilding
raco make -v main.rkt
cd ../time-regraph
racket time-regraph.rkt --rebuild
