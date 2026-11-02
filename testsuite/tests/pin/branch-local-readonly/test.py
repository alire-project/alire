"""
Check `alr update` does not fail on a local branch pin with a read-only origin.
"""

import os
import shutil
import stat
import sys
from pathlib import Path

from drivers.alr import run_alr, alr_pin, alr_unpin, init_local_crate
from drivers.asserts import assert_substring
from drivers.helpers import git_branch, init_git_repo, on_windows


# We can't chmod a directory to read-only on Windows, or as root.
if skip := (
    "on Windows" if on_windows()
    else "for root" if os.getuid() == 0
    else None
):
    print(f"SKIP: directory permissions are not enforced {skip}")
    sys.exit(0)


pin_dir = Path("alire", "cache", "pins", "upstream")
marker = pin_dir / "marker.txt"

# Initialise two "remote" git repos.
init_local_crate(name="upstream", enter=False)
init_git_repo("upstream")
branch = git_branch("upstream")
upstream = Path.cwd() / "upstream"
upstream_other = Path.cwd() / "upstream_other"
shutil.copytree(upstream, upstream_other)

# Pinning a client crate to the first upstream deploys the checkout.
init_local_crate("client")
alr_pin("upstream", url=str(upstream), branch=branch)
assert pin_dir.is_dir()

# Leave a telltale that will not survive a fresh clone.
marker.write_text("still here\n")

# Point the pin at the other upstream, without updating yet.
alr_unpin("upstream", update=False)
alr_pin("upstream", url=str(upstream_other), branch=branch, update=False)

# Make the old upstream read-only and update.
mode = stat.S_IMODE(upstream.stat().st_mode)
os.chmod(upstream, stat.S_IRUSR | stat.S_IXUSR)
try:
    p = run_alr("update", quiet=False)
finally:
    os.chmod(upstream, mode)

# The change of URL must result in a fresh clone.
assert_substring("Switching pin", p.out)
assert not marker.is_file()


print("SUCCESS")
