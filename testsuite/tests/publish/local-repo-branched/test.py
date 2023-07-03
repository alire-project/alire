"""
Test that publishing from a branch other than the default one works
"""

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match
from drivers.helpers import init_git_repo
from glob import glob
from shutil import copyfile
from subprocess import run

import os

# Prepare our "remote" repo
init_local_crate("xxx", enter=False)
head_commit = init_git_repo("xxx")

# Clone to a "local" repo and set minimal config
assert run(["git", "clone", "xxx", "xxx_local"]).returncode == 0
os.chdir("xxx_local")
assert run(["git", "config", "user.email", "alr@testing.com"]).returncode == 0
assert run(["git", "config", "user.name", "Alire Testsuite"]).returncode == 0

# Check out a new branch, add some file and push it to the "remote"
assert run(["git", "checkout", "-b", "devel"]).returncode == 0
copyfile("alire.toml", "noise.bin")
assert run(["git", "add", "."]).returncode == 0
assert run(["git", "commit", "-m", "commit-msg"]).returncode == 0
assert run(["git", "push", "-u", "origin", "devel"]).returncode == 0

# Check that the publishing assistant completes without complaining
run_alr("--force", "publish", "--skip-submit")

print('SUCCESS')
