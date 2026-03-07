"""
Verify that alr can run with no HOME set, or with it set to an
unwritable/nonexistent directory.
NOTE: This same test is duplicated to be run under docker as a regular user.
"""

import os
from drivers.alr import run_alr


def check_no_home():
    # Verify that alr is using the fallback home directory.
    p = run_alr("--format", "version")
    assert """{
    "key": "home folder",
    "value": "/tmp"
  }""" in p.out, "Unexpected output: " + p.out


# Remove HOME
os.environ.pop("HOME", None)
check_no_home()

# Set HOME to a nonexistent directory
fake_home = "/tmp/fake_home_for_alr"
assert not os.path.exists(fake_home)
os.environ["HOME"] = fake_home
check_no_home()

# Set HOME to an unwritable directory
# Under our docker, we should be a regular user and this should succeed
os.makedirs(fake_home)
os.chmod(fake_home, 0o444)
check_no_home()
# Cleanup
os.chmod(fake_home, 0o777)
os.rmdir(fake_home)


print("SUCCESS")
