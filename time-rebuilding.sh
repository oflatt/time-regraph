

cd ../regraph
git checkout rebuilding
git pull
raco make -v main.rkt
cd ../time-regraph
racket time-regraph.rkt --rebuild
