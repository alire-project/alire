"""
Verify that the index manifest is copied in the proper place instead of the one
possibly packed by upstream, like it is done for regular crates.
"""

from glob import glob
import os
from subprocess import run

from drivers.alr import alr_manifest, alr_publish, init_local_crate, run_alr
from drivers.asserts import assert_contents
from drivers.helpers import init_git_repo

# We create a repository with a nested crate that will act as the upstream
# remote repository:

start_dir = os.getcwd()
os.mkdir("monoproject.upstream")
os.chdir("monoproject.upstream")
init_local_crate("crate1", enter=False)
os.chdir(start_dir)
commit1 = init_git_repo("monoproject.upstream")

# We clone the project to obtain our local copy

assert run(["git", "clone",
            "monoproject.upstream", "monoproject"]).returncode == 0

# We enter the crate nested inside and publish.

os.chdir("monoproject")
os.chdir("crate1")
alr_publish("crate1", "0.1.0-dev",
            index_path=os.path.join(start_dir, "my_index"))

# Verify that we can `alr get` the nested crate

os.chdir(start_dir)
run_alr("get", "crate1")

# Enter and verify the only manifest is at the expected location (in the nested
# crate), and the only backup is too there.

os.chdir(glob("monoproject_*")[0])

assert not os.path.isfile(alr_manifest()), \
    "Unexpected manifest at the root of the repository"

assert_contents(".",
                ['./crate1/alire.toml',
                 './crate1/alire/alire.toml.upstream'],
                regex=".*alire.*toml")

print('SUCCESS')
