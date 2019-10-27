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

# Set up index if not default:
if [ "$INDEX" != "" ]; then
    echo Setting default index to: $INDEX
    alr index --name default --add "$INDEX"
fi

echo ALR SEARCH:
# List releases for the record
alr search -d --list --native
echo ............................

echo TESTSUITE:
# Run e3.testsuite
echo
cd testsuite
python --version
pip --version
pip install e3-testsuite
python ./run.py || { echo Test suite failures, unstable build!; exit 1; }
cd ..
echo ............................

# Check installer in stable branch
if [ "$BRANCH" == "stable" ]; then 
    echo -e '\n\n/bin\ny' | ./install/alr-bootstrap.sh
fi
