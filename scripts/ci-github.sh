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

# Disable distro detection if supported
if [ "${ALIRE_DISABLE_DISTRO:-}" == "true" ]; then
   alr config --global --set distribution.disable_detection true
fi

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

# On Windows, python3/pip3 don't explicitly exist
if [ "${OS:-}" == "Windows_NT" ]; then
    run_python=python
    run_pip=pip
else
    run_python=python3
    run_pip=pip3
fi

echo Python version: $($run_python --version)
echo Pip version: $($run_pip --version)

$run_pip install --upgrade e3-testsuite
echo Python search paths:
$run_python -c "import sys; print('\n'.join(sys.path))"

echo Running test suite now:
$run_python ./run.py -E || { echo Test suite failures, unstable build!; exit 1; }
cd ..
echo ............................
