"""
Check pinning to a branch, and changing branches
"""

from drivers.alr import run_alr, alr_pin, alr_unpin, init_local_crate
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import git_branch, git_head, init_git_repo, touch
from re import escape

import re
import os
import shutil
import subprocess

#  "remote" is going to be the remote crate

init_local_crate(name="remote", enter=False)
url = os.path.join(os.getcwd(), "remote")
head1 = init_git_repo("remote")
os.chdir("remote")
default_branch = git_branch()

# Create a second branch and commit for testing
subprocess.run(["git", "checkout", "-b", "devel"]).check_returncode()
touch("telltale")
subprocess.run(["git", "add", "telltale"]).check_returncode()
subprocess.run(["git", "commit", "-m", "branching"]).check_returncode()
os.chdir("..")

# Now pin to the branch, and verify the telltale file exists in the checkout
init_local_crate()
alr_pin("remote", url=url, branch="devel")
p = run_alr("pin")
assert_match("remote file:alire/cache/pins/remote " +
             re.escape(url) + "#devel\n",  # branch in the info matches
             p.out)

# Also verify the file exists
assert os.path.exists("alire/cache/pins/remote/telltale"), \
    "Missing file in checkout"

# Edit pin to point to the default branch, and verify telltale is missing
alr_unpin("remote", update=False)
alr_pin("remote", url=url, branch=default_branch)
p = run_alr("pin")
assert_match("remote file:alire/cache/pins/remote " +
             re.escape(url) + f"#{default_branch}\n",
             p.out)
assert not os.path.exists("alire/cache/pins/remote/telltale"), \
    "Unexpected file in checkout"

print('SUCCESS')
