"""
Test that a crate with incomplete config (values without defaults) cannot be
built/hashed
"""

from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.builds import hash_input

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
