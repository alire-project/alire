#!/usr/bin/env bash
# This script dispatches to the Ada patcher, after building it.

set -o errexit

# Exit already if the ALR_VERSION_DONT_PATCH flag is defined
if [ "${ALR_VERSION_DONT_PATCH:-unset}" != "unset" ]; then
    echo "Skipping version patching..."
    exit 0
fi

bin=support/version_patcher/bin/version_patcher

# If the binary is already in place, do nothing
if [ -f $bin ]; then
    echo "Patcher already built."
elif (which gprbuild &>/dev/null); then
    echo "Building patcher with gprbuild..."
    gprbuild -P support/version_patcher/version_patcher.gpr
elif (which alr &>/dev/null); then
    echo "Building patcher with alr..."
    alr -C "$(dirname $(dirname $bin))" build
else
    echo "WARNING: No Ada tool available to build patcher, skipping."
    exit 0
fi

$bin "$@"

echo "Resulting version file:"
cat src/alire/alire-version.ads | grep "Current_Str : constant String"