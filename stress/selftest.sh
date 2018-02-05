#!/bin/bash

set -o errexit
set -o nounset

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR
trap 'echo "Interrupted" >&2 ; exit 1' INT

if [[ "${1:-release}" == "devel" ]]; then
    echo Testing DEVEL version, press enter
    alias alr="$HOME/local/alr/bin/alr"
else
    echo Testing RELEASE version, press enter
    alias alr="$HOME/.config/alire/alr/bin/alr"
fi
read

workspace=/tmp/alrtest

rm -rf $workspace
mkdir $workspace
cd $workspace

# BUILD test
alr get hello && cd hello*
alr build 
alr build --online

# CLEAN test
alr clean

# COMPILE 
alr compile
cd ..
rm -rf hello*
alr get hello
cd hello*
alr compile

# GET
cd ..
rm -rf hello*
alr get --compile hello

# END
echo PASSED with crawling colors
