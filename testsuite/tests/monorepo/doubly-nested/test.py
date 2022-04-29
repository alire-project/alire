"""
Publish a doubly-nested crate: ./gitrepo/parent/child and test using it
"""

import os
import shutil

from drivers.alr import run_alr, init_local_crate, alr_with, alr_publish
from drivers.helpers import init_git_repo
from subprocess import run

# We create a repository with the nested crate that will act as the upstream
# remote repository:
start_dir = os.getcwd()
index_dir = os.path.join(os.getcwd(), "my_index")
os.mkdir("monoproject.upstream")
os.chdir("monoproject.upstream")
init_local_crate("myparent")
init_local_crate("mychild")
os.chdir(start_dir)
commit = init_git_repo("monoproject.upstream")

# We clone the project to obtain our local copy
assert run(["git", "clone",
            "monoproject.upstream", "monoproject"]).returncode == 0

# We enter the crate nested inside and publish.
os.chdir(os.path.join("monoproject", "myparent", "mychild"))
run_alr("show")  # Verify the crate is detected properly

# This call publishes and "submits" the release to our local index
alr_publish("mychild", "0.1.0-dev", index_path=index_dir)

# Publish also its parent for later test
os.chdir("..")
alr_publish("myparent", "0.1.0-dev", index_path=index_dir)

# Verify that the crate can be got and compiled, and expected location
os.chdir(start_dir)
run_alr("get", "--build", "mychild")
assert os.path.isdir(os.path.join(f"monoproject_{commit[:8]}",
                                  "myparent", "mychild")), \
    "Expected directory does not exist"

# Verify that the crate is usable as a dependency, and expected location
init_local_crate("top")
alr_with("mychild")
run_alr("build")
assert os.path.isdir(os.path.join("alire", "cache", "dependencies",
                                  f"monoproject_{commit[:8]}",
                                  "myparent", "mychild")), \
    "Expected directory does not exist"

# Verify that "with"ing the parent does not result in a new checkout
alr_with("myparent", update=False)
p = run_alr("-v", "update", quiet=False)
assert "Skipping checkout of already available myparent=0.1.0-dev" in p.out, \
    "Expected output not found: " + p.out

# Verify the build is successful. As the dependencies were created with --bin,
# they will be built even if not "with"ed in the Ada code to make their binary
# available to the root crate during the build process.
run_alr("build")

print('SUCCESS')
