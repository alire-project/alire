#!/usr/bin/env bash

# Import reusable bits
pushd "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )" > /dev/null || exit 1
    . functions.sh
popd > /dev/null || exit 1

ALIRE_BUILD_JOBS="${ALIRE_BUILD_JOBS:-0}"
ALIRE_OS=$(get_OS); export ALIRE_OS

scripts/version-patcher.sh

echo "Building with ALIRE_OS=$ALIRE_OS and $(gnat --version | head -1)"
gprbuild "-j$ALIRE_BUILD_JOBS" -r -p -P "$(dirname $0)/../alr_env.gpr" "$@"

scripts/version-patcher.sh _or_later
