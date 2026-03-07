"""
Check that broken/recursive symlinks don't cause alr to fail
"""

import os
from drivers.alr import run_alr, init_local_crate
# from drivers.asserts import assert_eq, assert_match

init_local_crate()

# Create a symbolic link to itself. This used to cause alr to fail.
os.symlink("self", "self")

# Commands that traverse looking for things (crates, executables) shouldn't
# fail.

run_alr("clean", "--temp")
run_alr("run")
run_alr("run", "--list")
run_alr("show", "--nested")

# Remove the symlink, otherwise it breaks the testsuite driver
os.unlink("self")

print("SUCCESS")