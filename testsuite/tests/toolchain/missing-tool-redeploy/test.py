"""
Verify that redeploying a missing tool works as intended.
As we cache the detected toolchains, if we fail to reload them on changes, we
could end trying to use a toolchain that is no longer available.
"""

import os
from shutil import rmtree
from drivers.alr import init_local_crate, run_alr

# We can trigger a buggy situation by configuring a toolchain, removing
# manually one tool (as if we had moved the cache), and running `alr printenv`.

run_alr("toolchain", "--select", "gprbuild", "gnat_native")
init_local_crate()

# Remove the tool manually through the filesystem
rmtree(os.path.join(os.environ["ALR_CONFIG"], "cache"))

# This should not fail
run_alr("printenv")


print("SUCCESS")
