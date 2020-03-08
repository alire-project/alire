#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

export PATH+=:${PWD}/bin


# For the record
echo ENVIRONMENT:
set | sort
echo ............................

echo GNAT VERSION:
gnatls -v
echo ............................

echo GCC VERSION/MACHINE
gcc -v
gcc -dumpmachine
echo ............................

# Build alr
case $OSTYPE in
   linux-gnu) export ALIRE_OS=linux;;
   msys)      export ALIRE_OS=windows;;
   darwin*)   export ALIRE_OS=macos;;
   *)
      echo Unsupported host OS: OSTYPE=$OSTYPE
      exit 1;;
esac

echo Building with ALIRE_OS=$ALIRE_OS ...
gprbuild -j0 -p -P alr_env

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
