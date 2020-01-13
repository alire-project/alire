#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

export PATH+=:${PWD}/bin

# For Darwin, have to define OS=macOS for alr_env.gpr
# Windows defines it anyway
# Linux (undefined) selects the default

[ `uname -s` == "Darwin" ] && export OS=macOS

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
if [ "${INDEX:-}" != "" ]; then
    echo Setting default index to: $INDEX
    alr index --name default --add "$INDEX"
fi

echo ALR SEARCH:
# List releases for the record
alr search -q -d --list --external
echo ............................

echo TESTSUITE:
# Run e3.testsuite
echo
cd testsuite
if [ "${OS:-}" == "Windows_NT" ]; then
    # There is some mixup of Python versions between the one installed by
    # Chocolatey and the one hosted in the github VM. To be looked into.
    run_python=python
    run_pip=pip
else
    run_python=python2
    run_pip=pip2
fi

$run_python --version
$run_pip --version
$run_pip install e3-testsuite
echo Python search paths:
$run_python -c "import sys; print('\n'.join(sys.path))"

echo Running test suite now:
$run_python ./run.py || { echo Test suite failures, unstable build!; exit 1; }
cd ..
echo ............................

# Check installer in stable branch
if [ "$BRANCH" == "stable" ]; then
    echo -e '\n\n/bin\ny' | ./install/alr-bootstrap.sh
fi
