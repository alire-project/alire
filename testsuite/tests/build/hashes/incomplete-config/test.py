"""
Test that a crate with incomplete config (values without defaults) cannot be
built/hashed
"""

import shutil
from drivers.alr import alr_with, external_compiler_version, init_local_crate, run_alr
from drivers.builds import find_hash, hash_input
from drivers.asserts import assert_eq, assert_match
from drivers import builds

run_alr("config", "--set", "--global", "dependencies.shared", "true")

init_local_crate()
alr_with("libhello=0.9")

run_alr("build", complain_on_error=False)
# This must fail as there are unset config varibles, but what really we are
# interested in is in checking that no build folder with an incorrect hash has
# been generated

try:
    hash = hash_input("libhello") # Must fail because no build folder exists
    assert False, "Build folder with incomplete config should not be hashed"
except AssertionError:
    pass

print("SUCCESS")
