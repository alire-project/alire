"""
Test conversion of git references into commits during pinning
"""

from drivers.alr import run_alr, alr_with, init_local_crate
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import init_git_repo

import os
import subprocess

# Create a new "remote" repository with a tag that we'll use as reference
init_local_crate("remote")
init_git_repo(".")
subprocess.run(["git", "tag", "v1"]).check_returncode()

# Verify that pinning to a valid reference succeeds
os.chdir("..")
init_local_crate()
alr_with(path="../remote", commit="v1", manual=False)
p = run_alr("pin")
assert_match("remote file:alire/cache/pins/remote_.{,8} ../remote#.{,8}",
             p.out)

# Remove dependency for next test
alr_with("remote", delete=True, manual=False)
p = run_alr("pin")
assert_eq("There are no pins\n", p.out)

# Verify that pinning to a valid reference also succeeds with the --arg=value
# form
run_alr("with", "--use=../remote", "--commit=v1")
p = run_alr("pin")
assert_match("remote file:alire/cache/pins/remote_.{,8} ../remote#.{,8}",
             p.out)

# Remove dependency for next test
alr_with("remote", delete=True, manual=False)
p = run_alr("pin")
assert_eq("There are no pins\n", p.out)

# Verify that pinning to a valid reference also succeeds with an explicit URL
alr_with(url="git+file:../remote", commit="v1", manual=False)
p = run_alr("pin")
assert_match(
    r"remote file:alire/cache/pins/remote_.{,8} git\+file:\.\./remote#.{,8}",
    p.out
)

# Remove dependency for next test
alr_with("remote", delete=True, manual=False)
p = run_alr("pin")
assert_eq("There are no pins\n", p.out)

# Verify that pinning to an invalid reference fails
p = run_alr("with", "--use", "../remote", "--commit", "v2",
            complain_on_error=False)
assert_match(".*Requested remote reference v2 not found in repository.*",
             p.out)

# Verify that pinning to an invalid reference also fails with the
# --arg=value form
p = run_alr("with", "--use=../remote", "--commit=v2",
            complain_on_error=False)
assert_match(".*Requested remote reference v2 not found in repository.*",
             p.out)

# Verify that pinning to an invalid reference also fails with an explicit URL
p = run_alr("with", "--use=git+file:../remote", "--commit=v2",
            complain_on_error=False)
assert_match(".*Requested remote reference v2 not found in repository.*",
             p.out)

print('SUCCESS')
