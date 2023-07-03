"""
Test proper publishing using a local repo as reference with custom manifest
"""

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match
from drivers.helpers import init_git_repo
from glob import glob
from shutil import rmtree
from subprocess import run

import os


def verify_manifest():
    target = os.path.join("alire", "releases", "xxx-0.1.0-dev.toml")
    assert os.path.isfile(target), \
        "Index manifest not found at expected location"
    # Clean up for next test
    rmtree(os.path.join("alire", "releases"))


# Prepare our "remote" repo, changing the manifest name to "xxx.toml"
init_local_crate("xxx")
os.rename("alire.toml", "xxx.toml")
os.chdir("..")
head_commit = init_git_repo("xxx")

# Clone to a "local" repo and set minimal config
assert run(["git", "clone", "xxx", "xxx_local"]).returncode == 0
os.chdir("xxx_local")
assert run(["git", "config", "user.email", "alr@testing.com"]).returncode == 0
assert run(["git", "config", "user.name", "Alire Testsuite"]).returncode == 0

# Tests with different default arguments that must all succeed
run_alr("--force", "publish", "--skip-submit", "--manifest", "xxx.toml")
verify_manifest()

run_alr("--force", "publish", "--skip-submit", ".", "--manifest", "xxx.toml")
verify_manifest()

run_alr("--force", "publish", "--skip-submit", ".", "master", "--manifest", "xxx.toml")
verify_manifest()

run_alr("--force", "publish", "--skip-submit", ".", "HEAD", "--manifest", "xxx.toml")
verify_manifest()

# Test that not setting the custom manifest results in failure
p = run_alr("--force", "publish", "--skip-submit", complain_on_error=False)
assert_match(".*No Alire workspace found.*", p.out)

print('SUCCESS')
