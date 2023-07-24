#!/usr/bin/env bash

# Import reusable bits
pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd ) > /dev/null
    . functions.sh
popd > /dev/null

export ALIRE_OS=$(get_OS)

echo Building with ALIRE_OS=$ALIRE_OS...
gprbuild -j0 -r -p -P `dirname $0`/../alr_env.gpr "$@"
