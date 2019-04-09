#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR 
trap 'echo "Interrupted" >&2 ; exit 1' INT 

set -o errexit
set -o nounset

if [ "$BRANCH" == "master" ]; then 
    echo -e '\n\n/bin\ny' | ./install/alr-bootstrap.sh
#    alr update --online # until #73 is fixed
else
    gprbuild -j0 -p -P alr_env
    export PATH+=:`pwd`/bin
fi 

return 0 # until #73 is fixed

alr search -d --list --native

bash stress/selftest.sh docker

mkdir reltest selftest
cd selftest
alr dev --test
cp *.xml ../shippable/testresults
cd ../reltest 
alr test --full --newest
cp *.xml ../shippable/testresults 
