"""
Verify that build/run using --chdir doesn't cause an unexpected change of
profile (and hence a rebuild). This was issue #2015.
"""

import os
from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_in_file

# We will initialize a crate and build it in non-default profile (f.e. release)
# Then we run it and observe that the profile hasn't changed

TELLTALE = "Build_Profile : constant Build_Profile_Kind := release;"


def assert_profile(prefix="xxx"):
    """
    Check that the profile in the configuration file is release
    """
    assert_in_file(os.path.join(prefix, "config", "xxx_config.ads"), TELLTALE)


init_local_crate()
run_alr("build", "--release")

# Check the release profile in configuration
assert_profile(prefix=".")

# Pull out and run from outside
os.chdir("..")
run_alr("--chdir=xxx", "run")
assert_profile()
run_alr("-C", "xxx", "run")
assert_profile()

# As a bonus, test . and .. (the check is new in this bugfix)
run_alr("--chdir=./xxx", "run")
assert_profile()
run_alr("--chdir=xxx/../xxx", "run")
assert_profile()
run_alr(f"--chdir={os.getcwd()}/xxx/.", "run")
assert_profile()

print("SUCCESS")
