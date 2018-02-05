#!/bin/bash

set -o errexit
set -o nounset

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

if [[ "${1:-release}" == "devel" ]]; then
    echo Testing DEVEL version, press enter
    touch ~/.config/alire/enable-devel
else
    echo Testing RELEASE version, press enter
    rm -f ~/.config/alire/enable-devel
fi
read

workspace=/tmp/alrtest

rm -rf $workspace
mkdir $workspace
pushd $workspace

# BUILD test
echo BUILD
alr get hello
pushd hello*
alr build 
alr build --online
popd

# CLEAN test
pushd hello*
echo CLEAN
alr clean
popd

# COMPILE 
echo COMPILE
pushd hello*
alr compile
popd
rm -rf hello*
alr get hello
pushd hello*
alr compile
popd
rm -rf hello*
alr get -c libhello
rm -rf libhello

# GET
echo GET
alr get --compile hello
rm -rf hello*

# HELP
echo HELP
alr help build | grep -q online

# INIT
echo INIT
alr init --bin xxx
rm -rf xxx
alr init --lib xxx
rm -rf xxx
alr init --bin -b xxx
rm -rf xxx
alr init --lib -b xxx
rm -rf xxx

# LOCK
echo LOCK
alr init --bin xxx
cd xxx
grep -q -i At_Least xxx_alr.ads
alr lock
grep -q -i Exactly xxx_alr.ads
cd ..
rm -rf xxx

# RUN
echo RUN
alr get hello
cd hello*
# Next should fail since there's no exec, hence not failing the test
alr run -n >/dev/null && { echo FAIL ; exit 1; } || echo Run without exec PASSED
alr run | grep -q -i hello
cd ..
rm -rf hello*

# SEARCH
echo SEARCH
alr search --list | grep -q -i hello
alr search hell | grep -q -i hello

# UPDATE
echo UPDATE
# outisde
alr update --online | tee /dev/tty | grep -i -q Done
# inside
alr init --bin xxx
cd xxx
alr update | tee /dev/tty | grep -i -q completed
cd ..
rm -rf xxx

# END
echo ' '
echo PASSED with crawling colors
