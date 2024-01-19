"""
Verify that redeploying a missing tool works as intended.
As we cache the detected toolchains, if we fail to reload them on changes, we
could end trying to use a toolchain that is no longer available.
"""

import os
from shutil import rmtree
from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match

# We can trigger a buggy situation by configuring a toolchain, removing
# manually the tool (as if we had moved the cache), and running `alr printenv`.

run_alr("toolchain", "--select", "gprbuild", "gnat_native")
init_local_crate()

# Remove the tool manually through the filesystem
rmtree(os.path.join(os.environ["ALR_CONFIG"], "cache"))

# This should not fail. A message should warn of redeployments happening.
p = run_alr("printenv", quiet=False)
assert_match(".*Tool .* is missing, redeploying", p.out)

print("SUCCESS")
