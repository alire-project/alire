"""
Publish two crates and use them as dependencies, but at different commits of
the same monorepo. The monorepo must be checked out twice and the duplicated
crate must not cause trouble.
"""

import os
from subprocess import run

from drivers import builds
from drivers.alr import alr_publish, alr_with, init_local_crate, run_alr
from drivers.helpers import commit_all, init_git_repo, on_windows

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


def release_base(commit: str) -> str:
    if builds.are_shared():
        return builds.find_dir(f"monoproject_{commit[:8]}")
    else:
        return os.path.join("alire", "cache", "dependencies",
                            f"monoproject_{commit[:8]}")


assert os.path.isfile(os.path.join(
    release_base(commit1), "crate1", "bin",
    f"crate1{'.exe' if on_windows() else ''}")), \
    "Expected binary does not exist"
assert os.path.isfile(os.path.join(
    release_base(commit2), "crate2", "bin",
    f"crate2{'.exe' if on_windows() else ''}")), \
    "Expected binary does not exist"

# Also that the info files are there
if builds.are_shared():
    deps_dir = builds.vault_path()
else:
    deps_dir = os.path.join("alire", "cache", "dependencies")

assert os.path.isfile(os.path.join(
    deps_dir,
    f"crate1_0.1.0_in_monoproject_{commit1[:8]}")), \
    f"Expected info file does not exist"
assert os.path.isfile(os.path.join(
    deps_dir,
    f"crate2_0.1.0_in_monoproject_{commit2[:8]}")), \
    "Expected info file does not exist"

print('SUCCESS')
