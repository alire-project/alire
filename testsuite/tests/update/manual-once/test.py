"""
Verify that, after manually touching the manifest, updates happen only once
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq, assert_match

import os

# There are two scenarios to test: both start with a manual edition of the
# manifest. Afterwards, the user can either run 'update', which goes straight
# into applying any changes found; or, they can run any other command, which
# should also check for changes before doing anything, so the lockfile is in
# sync with the manifest. We test both here.


def prepare_crate(name):
    """Prepare a crate with outdated lockfile"""
    init_local_crate(name)
    # Set the modification time of the lockfile behind that of the manifest
    info = os.stat("alire.toml")
    os.utime("alire.lock", (info.st_atime, info.st_mtime - 1))

warning_text = "Synchronizing workspace"

# Test when directly doing an update. Should report no changes.
prepare_crate("test1")
p = run_alr("update", quiet=False)
assert_eq("Nothing to update.\n", p.out)
# Also check that the modified manifest warning is not shown, as we are
# requesting the update explicitly:
assert warning_text not in p.out

# Test when doing other things. Should warn once of possible changes.
prepare_crate("test2")
p = run_alr("with", quiet=False)  # First run must warn
assert_match(".*" + warning_text + ".*", p.out)
p = run_alr("with", quiet=False)  # Second run must not warn
assert warning_text not in p.out


print('SUCCESS')
