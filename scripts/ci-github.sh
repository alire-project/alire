#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR 
trap 'echo "Interrupted" >&2 ; exit 1' INT 

set -o errexit
set -o nounset

export PATH+=:${PWD}/bin

# Build alr
gprbuild -j0 -p -P alr_env

# For the record
echo ENVIRONMENT:
env | sort
echo ............................

echo GNAT VERSION:
gnatls -v
echo ............................

echo ALR VERSION:
alr version
echo ............................

# List releases for the record
alr search -d --list --native

# Run e3.testsuite
echo
cd testsuite
./run.py || { echo Test suite failures, unstable build!; exit 1; }
cd ..

# Check installer in stable branch
if [ "$BRANCH" == "stable" ]; then 
    echo -e '\n\n/bin\ny' | ./install/alr-bootstrap.sh
fi
