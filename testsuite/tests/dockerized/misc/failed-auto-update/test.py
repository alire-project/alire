"""
Check proper behavior when an index auto-update fails due to a network error
"""

import os
import subprocess
from subprocess import run
import sys

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Configure our online test index
# An alternative might be to launch a local git server in bg
INDEX = "git+https://github.com/alire-project/test-index"
run_alr("index", "--name", "test", "--add", INDEX)

# Check that everything works as expected without index auto-update
run_alr("show", "libhello")

# Enable autoupdate
run_alr("settings", "--global", "--set", "index.auto_update", "1")

# Ensure next `alr show` will trigger an update
run_alr("settings", "--global", "--unset", "index.last_update")
run_alr("settings", "--global", "--set", "index.auto_update_asked", "true")

# Prepare a copy of `unshare` that can be run as regular user
run(["cp", "/usr/bin/unshare", "/tmp"]).check_returncode()
run(["sudo", "setcap", "cap_sys_admin+ep", "/tmp/unshare"]).check_returncode()

# This command should not fail, but print a warning. Unshare makes the network
# unavailable to alr, so the community index update will fail.
p = run(["/tmp/unshare", "-n", "alr", "-n", "show", "libhello"],
                   stdout=subprocess.PIPE,
                   stderr=subprocess.STDOUT,
                   text=True)

assert p.returncode == 0, f"UNEXPECTED EXIT CODE: {p.returncode}: {p.stdout}"
assert_match(".*Warning: Index auto-refresh failed, will try again in 1h"
             , p.stdout)

# Running again should not produce the warning
p = run(["/tmp/unshare", "-n", "alr", "-n", "show", "libhello"],
                   stdout=subprocess.PIPE,
                   stderr=subprocess.STDOUT,
                   text=True)
assert p.returncode == 0, f"UNEXPECTED EXIT CODE: {p.returncode}: {p.stdout}"
assert "Index auto-refresh failed" not in p.stdout, \
    f"UNEXPECTED OUTPUT: {p.stdout}"


print('SUCCESS')
