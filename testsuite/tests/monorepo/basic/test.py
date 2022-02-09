"""
Publish a crate that is nested inside a git repository, get/build it and also
use it as a dependency
"""

import os
import shutil

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.helpers import init_git_repo
# from drivers.asserts import assert_eq, assert_match
from subprocess import run

# We create a repository with the nested crate that will act as the upstream
# remote repository:
start_dir = os.getcwd()
os.mkdir("monoproject.upstream")
os.chdir("monoproject.upstream")
init_local_crate("mycrate", enter=False)
os.chdir(start_dir)
commit = init_git_repo("monoproject.upstream")

# We clone the project to obtain our local copy
assert run(["git", "clone",
            "monoproject.upstream", "monoproject"]).returncode == 0

# We enter the clone and verify that attempting to publish from the root fails,
# as there is no crate at the git root
os.chdir("monoproject")
p = run_alr("publish", complain_on_error=False)
assert p.status != 0, "alr failed to err as expected"
assert "No Alire workspace found" in p.out, "Unexpected output: " + p.out

# We enter the crate nested inside and publish.
# We are now at monoproject/mycrate.
os.chdir("mycrate")
run_alr("show")  # Verify the crate is detected properly
run_alr("publish", force=True)  # Force due to missing optional crate info

# Prepare destination at index
os.chdir(start_dir)
os.makedirs(os.path.join("my_index", "index", "my", "mycrate"))

# Move published manifest to proper index location
os.rename(os.path.join("monoproject", "mycrate",
                       "alire", "releases", "mycrate-0.0.0.toml"),
          os.path.join("my_index", "index",
                       "my", "mycrate", "mycrate-0.0.0.toml"))

# Verify that the crate can be got and compiled, and expected location
run_alr("get", "--build", "mycrate")
assert os.path.isdir(os.path.join(f"monoproject_{commit[:8]}", "mycrate")), \
    "Expected directory does not exist"

# Verify that the crate is usable as a dependency, and expected location
init_local_crate("top")
alr_with("mycrate")
run_alr("build")
assert os.path.isdir(os.path.join("alire", "cache", "dependencies",
                                  f"monoproject_{commit[:8]}", "mycrate")), \
    "Expected directory does not exist"

print('SUCCESS')
