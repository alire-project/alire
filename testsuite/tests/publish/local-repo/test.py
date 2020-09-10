"""
Tests for proper publishing using a local repo as reference
"""

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match
from drivers.helpers import init_git_repo
from shutil import rmtree
from subprocess import run

import os


def verify_manifest():
    target = os.path.join("alire", "releases", "xxx-0.0.0.toml")
    assert os.path.isfile(target), \
        "Index manifest not found at expected location"
    # Clean up for next test
    shutil.rmtree(os.path.join("alire", "releases"))


# Prepare our "remote" repo
init_local_crate("xxx", enter=False)
head_commit = init_git_repo("xxx")

# Clone to a "local" repo
assert run(["git", "clone", "xxx", "xxx_local"]).returncode == 0
os.chdir("xxx_local")

# Tests with different default arguments that must all succeed
run_alr("publish")
verify_manifest()

run_alr("publish", ".")
verify_manifest()

run_alr("publish", ".", "master")
verify_manifest()

run_alr("publish", ".", "HEAD")
verify_manifest()

# Verify that a dirty repo precludes publishing
with open("lasagna", "wt") as file:
    file.write("wanted\n")

p = run_alr("publish", complain_or_error=False)
assert_match(".*git status reports working tree not clean.*", p.out)

# Even if changes are commited but not pushed
assert run(["git", "add", "."]).returncode == 0
assert run(["git", "commit", "-a", "-m", "please"]).returncode == 0
p = run_alr("publish", complain_or_error=False)
assert_match(".*Repository has commits yet to be pushed.*", p.out)

# It works again after push
assert run(["git", "push"]).returncode == 0
run_alr("publish")

print('SUCCESS')
