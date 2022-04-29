"""
Publish two crates and use them as dependencies, but at different commits of
the same monorepo. The monorepo must be checked out twice and the duplicated
crate must not cause trouble.
"""

import os
import shutil

from drivers.alr import run_alr, init_local_crate, alr_with, alr_publish
from drivers.helpers import init_git_repo, on_windows, commit_all
# from drivers.asserts import assert_eq, assert_match
from subprocess import run

# We create a repository with two nested crates that will act as the upstream
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
# We are now at monoproject/mycrate.
os.chdir("monoproject")
os.chdir("crate1")
alr_publish("crate1", "0.1.0-dev", index_path=os.path.join(start_dir, "my_index"))

# We create a second crate at another commit
os.chdir(os.path.join(start_dir, "monoproject.upstream"))
init_local_crate("crate2", enter=False)
os.chdir(start_dir)
commit2 = commit_all("monoproject.upstream")

# Check out changes and publish crate2
os.chdir("monoproject")
run(["git", "pull"]).check_returncode()
os.chdir("crate2")
alr_publish("crate2", "0.1.0-dev", index_path=os.path.join(start_dir, "my_index"))

# Verify that the crates are usable as a dependency, and expected binary
# locations
init_local_crate("top")
alr_with("crate1")
alr_with("crate2")
run_alr("build")

assert os.path.isfile(os.path.join(
    "alire", "cache", "dependencies",
    f"monoproject_{commit1[:8]}", "crate1", "bin",
    f"crate1{'.exe' if on_windows() else ''}")), \
    "Expected binary does not exist"
assert os.path.isfile(os.path.join(
    "alire", "cache", "dependencies",
    f"monoproject_{commit2[:8]}", "crate2", "bin",
    f"crate2{'.exe' if on_windows() else ''}")), \
    "Expected binary does not exist"

# Also that the info files are there
assert os.path.isfile(os.path.join(
    "alire", "cache", "dependencies",
    f"crate1_0.1.0_in_monoproject_{commit1[:8]}")), \
    "Expected info file does not exist"
assert os.path.isfile(os.path.join(
    "alire", "cache", "dependencies",
    f"crate2_0.1.0_in_monoproject_{commit2[:8]}")), \
    "Expected info file does not exist"

print('SUCCESS')
