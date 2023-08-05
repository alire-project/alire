"""
Check that config is regenerated when dependencies are shared
"""

import os

from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_profile

deps_dir = "deps"

# Enable sharing of dependencies
os.mkdir(deps_dir)
run_alr("config", "--global", "--set", "dependencies.dir",
        os.path.abspath(deps_dir))

# Create a crate with a dependency
init_local_crate()
alr_with("libhello")

# Build normally and check build profile of dependency
run_alr("build")
os.chdir("..")
assert_profile(crate="libhello", profile="release",
               root=os.path.join(deps_dir, "libhello_1.0.0_filesystem"))

# Create second crate with the same dependency and build with debug profile
init_local_crate("yyy")
alr_with("libhello")
run_alr("build", "--profiles=libhello=development")

# And verify that the new profile has been applied
os.chdir("..")
assert_profile(crate="libhello", profile="development",
               root=os.path.join(deps_dir, "libhello_1.0.0_filesystem"))

# Go back to the first crate and recheck, as now we have a previous profile
# stored that normally wouldn't be regenerated
os.chdir("xxx")
run_alr("build")
assert_profile(crate="libhello", profile="release",
               root=os.path.join("..", deps_dir, "libhello_1.0.0_filesystem"))

print('SUCCESS')
