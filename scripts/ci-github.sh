#!/usr/bin/env bash

# Parameters (optional):
# skip_build   : skip alr build
# skip_test    : skip testsuites
# use_external : use the system external compiler for the Ada testsuite

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

set -o errexit
set -o nounset

export PATH+=:${PWD}/bin

# Import reusable bits
pushd "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
    . ../dev/functions.sh
popd

# Mark location safe to assuage git if necessary (happens under docker as we
# run with a different user).
if git status 2>&1 | grep -q "dubious ownership"; then
   echo "Marking $PWD as safe for git"
   git config --global --add safe.directory "$PWD"

   # Change ownership and group to current user of everything in the testsuite,
   # as we have there some pre-created git repositories that would fail too.
   # These are copied to temporary locations by the test runner, so we cannot
   # simply use the `git config` trick.
   sudo chown -R $(id -u):$(id -g) testsuite
fi

# Patch version
scripts/version-patcher.sh

# Use -static-libgcc only on macOS.
ALR_LINKER_ARGS=
if [ "$(get_OS)" == "macos" ]; then
    ALR_LINKER_ARGS="-static-libgcc"
fi

# Build alr if "skip_build" is not passed"
if [[ " $* " == *" skip_build "* ]]; then
    echo "Skipping alr build, explicitly disabled via arguments"
else
    export ALIRE_OS=$(get_OS)
    echo "Using ALR_LINKER_ARGS=$ALR_LINKER_ARGS"
    gprbuild -j0 -p -P alr_env -largs $ALR_LINKER_ARGS
fi

# Disable distro detection if supported
if [ "${ALIRE_DISABLE_DISTRO:-}" == "true" ]; then
   alr settings --global --set distribution.disable_detection true
fi

# For the record
echo ENVIRONMENT:
env | sort
echo ............................

echo GNAT VERSION:
gnatls -v
echo ............................

echo "ALR VERSION (at $(which alr)):"
alr -d version
echo ............................

# Set up index if not default:
if [ "${INDEX:-}" != "" ]; then
    echo Setting default index to: "$INDEX"
    alr index --name default --add "$INDEX"
fi

echo "ALR SETTINGS (global):"
alr settings --global
echo ............................

echo ALR SEARCH:
# List releases for the record, ensuring no cached interference
alr index --reset-community
alr -q -d search --list --external
echo ............................

# Exit without testing if some argument is "skip_test"
if [[ " $* " == *" skip_test "* ]]; then
    echo "SKIPPING testsuite, explicitly disabled via arguments"
    exit 0
fi

echo TESTSUITE:
# Run e3.testsuite
echo
cd testsuite

# On Windows, python3/pip3 don't explicitly exist. Also we don't need a venv.
if [ "${OS:-}" == "Windows_NT" ]; then
    run_python=python
    run_pip=pip
else
    run_python=python3
    run_pip=pip3
    # Some distros complain that we are trying to install packages globally,
    # e.g. latest Debian, so use a virtualenv:
    $run_python -m venv venv && . venv/bin/activate
fi

echo PYTHON installing testsuite dependencies...

echo "Python version: $($run_python --version)"
echo "Pip version: $($run_pip --version)"

$run_pip install --upgrade -r requirements.txt
echo Python search paths:
$run_python -c "import sys; print('\n'.join(sys.path))"

echo Check Finalize exception handling :
$run_python ../scripts/python/check_finalize_exceptions.py ../src
echo ............................

echo Running Python test suite now:
$run_python ./run.py --show-time-info -E || { echo Python test suite failures, unstable build!; exit 1; }
cd ..
echo ............................

# Run Ada testsuite last as re-building alr in validation mode is
# time-consuming and we want to catch any issues with the Python testsuite
# first. Also, as we want to keep the already built alr for the artifacts and
# possible releases, we preserve the current alr and restore it afterwards.
# This also allows Windows to generate a new executable (otherwise it cannot
# overwrite the running binary).

# if use_external is passed, we select the system external compiler

if [[ " $* " == *" use_external "* ]]; then
    echo "Selecting external compiler"
    alr toolchain --select gnat_external gprbuild
fi

mkdir bak && cp bin/alr* bak
echo Running Ada test suite now:
bak/alr test || { echo Ada test suite failures, unstable build!; exit 1; }
rm -rf bin/alr* && mv bak/alr* bin
