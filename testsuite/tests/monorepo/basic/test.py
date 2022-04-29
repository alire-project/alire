"""
Publish a crate that is nested inside a git repository, get/build it and also
use it as a dependency
"""

import os
import shutil

from drivers.alr import run_alr, init_local_crate, alr_with, alr_publish
from drivers.helpers import init_git_repo, on_windows
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

# This call creates the manifest and puts it in place in the index
alr_publish("mycrate", "0.1.0-dev", index_path=os.path.join(start_dir, "my_index"))

# Verify that the crate can be got and compiled, and expected location
os.chdir(start_dir)
run_alr("get", "--build", "mycrate")
assert os.path.isdir(os.path.join(f"monoproject_{commit[:8]}", "mycrate")), \
    "Expected directory does not exist"

# Verify that the crate is usable as a dependency, and expected binary location
init_local_crate("top")
alr_with("mycrate")
run_alr("build")
assert os.path.isfile(os.path.join(
    "alire", "cache", "dependencies",
    f"monoproject_{commit[:8]}", "mycrate", "bin",
    f"mycrate{'.exe' if on_windows() else ''}")), \
    "Expected binary does not exist"

# Also that the info file is there
assert os.path.isfile(os.path.join(
    "alire", "cache", "dependencies",
    f"mycrate_0.1.0_in_monoproject_{commit[:8]}")), \
    "Expected info file does not exist"

print('SUCCESS')
