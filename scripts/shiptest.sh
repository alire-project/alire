#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR 
trap 'echo "Interrupted" >&2 ; exit 1' INT 

set -o errexit
set -o nounset

# For the record
echo ENVIRONMENT:
env | sort
echo ............................

# Ensure subrepos are there
git submodule update --init --recursive

# Check compilation in all cases
gprbuild -j0 -p -P alr_env

# Check installer in stable branch
if [ "$BRANCH" == "stable" ] || [ "$BASE_BRANCH" == "stable" ]; then 
    echo -e '\n\n/bin\ny' | ./install/alr-bootstrap.sh
fi

export PATH+=:`pwd`/bin

echo GNAT VERSION:
gnatls -v
echo ............................

# This is temporary addition for this PR to pass CI tests with the new index.
# Remove once merged
alr index --name pro --add git+https://github.com/alire-project/alire-index@06ce3ef

echo ALR VERSION:
alr version
echo ............................

# List releases for the record
alr search -d --list --native

# Run e3.testsuite
echo
cd testsuite
./run.py --xunit-output ../shippable/testresults/e3-output.xml || echo Test suite failures, unstable build!
cd ..
