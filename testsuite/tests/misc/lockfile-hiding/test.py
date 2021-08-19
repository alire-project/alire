"""
Verify that lockfiles are properly [re]moved from $crate/ to $crate/alire/
"""

import os

from drivers.alr import run_alr, init_local_crate, alr_lockfile, alr_with
# from drivers.asserts import assert_eq, assert_match
from drivers.helpers import content_of
from os.path import isfile


old_lockfile = "alire.lock"

init_local_crate()
# Add a dependency so the lockfile is not the same as the default one
default_content = content_of(alr_lockfile())
alr_with("libhello")
proper_content = content_of(alr_lockfile())
assert default_content != proper_content, \
    "error setting up the test (contents should differ)"


def verify():
    """
    Check that the lockfile is only in the new location, with proper contents
    """
    assert isfile(alr_lockfile()) and not isfile(old_lockfile), \
        "Unexpected lockfile placement"
    assert content_of(alr_lockfile()) == proper_content, "bad contents"


# Case 1: lockfile is only in the old location, and it is moved to new location

# First, get the current lockfile and put it in the old location:
os.rename(alr_lockfile(), old_lockfile)
run_alr("show")  # Triggers migration
verify()

# Case 2: have an old lockfile that should be discarded without being copied,
# as the new lockfile is already in place (at the end of Case 1):
with open(old_lockfile, "wt") as file:
    file.write(default_content)
run_alr("show")
verify()


print('SUCCESS')
