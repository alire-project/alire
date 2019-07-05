#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR 
trap 'echo "Interrupted" >&2 ; exit 1' INT 

set -o errexit
set -o nounset

# Ensure subrepos are there
git submodule update --init --recursive

# Check compilation in all cases
gprbuild -j0 -p -P alr_env

# Check installer in stable branch
if [ "$BRANCH" == "stable" ] || [ "$BASE_BRANCH" == "stable" ]; then 
    echo -e '\n\n/bin\ny' | ./install/alr-bootstrap.sh
fi

export PATH+=:`pwd`/bin

# Check minimal execution
alr version

# List releases for the record
alr search -d --list --native

# Run e3.testsuite
echo
cd testsuite
./run.py --xunit-output ../shippable/testresults/e3-output.xml
cd ..
