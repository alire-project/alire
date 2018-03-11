#!/bin/bash

set -o errexit
set -o nounset

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

mkdir -p ~/.config/alire

version=${1:-release}

if [[ "$version" == "working" ]]; then
    echo Testing WORKING version, press enter
    pushd ~/opt/bin
    ln -sf ~/local/alr/bin/alr
    popd
    read
elif [[ "$version" == "release" ]]; then
    echo Testing RELEASE version, press enter
    pushd ~/opt/bin
    ln -sf ~/.config/alire/alr/bin/alr
    popd
    read
else
    # Let's presume it's a testing branch:
    echo About to test branch $version as release candidate, press enter
    read
    rm -rf ~/.config/alire
    mkdir -p ~/.config/alire
    git clone --recurse-submodules -n https://bitbucket.org/aleteolabs/alr ~/.config/alire/alr
    pushd ~/.config/alire/alr
    git checkout $version
    git submodule update --init --recursive
    gprbuild -p -P alr_env
    popd
    pushd ~/opt/bin
    ln -sf ~/.config/alire/alr/bin/alr
    popd
fi

workspace=/tmp/alrtest

rm -rf ~/.cache/alire
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
#alr init --bin -b xxx
#rm -rf xxx
#alr init --lib -b xxx
#rm -rf xxx

# PIN 
echo PIN 
alr init --bin xxx
cd xxx
grep -q -i Current xxx_alr.ads
alr pin
grep -q -i At_Version xxx_alr.ads
cd ..
rm -rf xxx

# RUN
echo RUN
alr get hello
cd hello*
# Next should fail since there's no exec, hence not failing the test
alr run -s >/dev/null && { echo FAIL ; exit 1; } || echo Run without exec PASSED
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
