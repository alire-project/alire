#!/usr/bin/env bash

# Import reusable bits
pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd ) > /dev/null || exit 1
    . functions.sh
popd > /dev/null || exit 1

export ALIRE_OS=$(get_OS)

gprclean -f -r -Palr_env.gpr
