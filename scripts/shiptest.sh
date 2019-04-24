#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR 
trap 'echo "Interrupted" >&2 ; exit 1' INT 

set -o errexit
set -o nounset

# Check compilation in all cases
gprbuild -j0 -p -P alr_env

# Check installer in stable branch
if [ "$BRANCH" == "stable" ]; then 
    echo -e '\n\n/bin\ny' | ./install/alr-bootstrap.sh
#    alr update --online # until #73 is fixed
fi

export PATH+=:`pwd`/bin

# Check minimal execution
alr version

# List releases for the record
alr search -d --list --native

# Minimal self-checks until new test suite
bash stress/selftest.sh docker

# Do not perform package tests yet
exit 0

mkdir reltest selftest
cd selftest
alr dev --test
cp *.xml ../shippable/testresults
cd ../reltest 
alr test --full --newest
cp *.xml ../shippable/testresults 
