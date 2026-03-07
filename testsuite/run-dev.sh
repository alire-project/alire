#!/usr/bin/env bash

# This script is used to run the testsuite with some extra tests enabled,
# intended only for the main developers in their local machines.

export ALR_TESTSUITE_ALLOW=1                  # So `alr` doesn't raise
export ALIRE_TESTSUITE_ENABLE_LOCAL_TESTS=1   # So they're actually run

clear

# Warn if GH_USERNAME is unset
if [ -z "$GH_USERNAME" ]; then
    echo "Warning: GH_USERNAME is unset, some tests will be skipped."
    echo "Continuing in 3 seconds..."
    sleep 3
    echo
fi

./run.py --show-time-info -M1 "$@"
