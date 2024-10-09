"""
Verify that using manual edition and `alr with` result in equivalent manifests
"""

import os

from e3.fs import rm

from drivers.alr import run_alr, alr_with, init_local_crate
from drivers.asserts import assert_eq
from drivers.helpers import init_git_repo, git_branch


def check_equivalent(dep="", path="", url="", commit="", branch=""):
    """
    Run manual and auto and compare outputs
    """
    manual = [False, True]
    p = [None, None]

    for i in [0, 1]:
        init_local_crate()

        # run command
        alr_with(dep=dep, path=path, url=url,
                 commit=commit, branch=branch, force=True,
                 manual=manual[i])

        # get output of solution
        p[i] = run_alr("with", "--solve").out

        if i == 1:
            assert_eq(p[0], p[1])

        # Cleanup
        os.chdir("..")
        rm("xxx", recursive=True)


# Simple with without subset cannot be tested as `alr with` will narrow down
# the dependency causing a discrepancy

# Existing crate with subset
check_equivalent("libhello^1")

# Non-existent version
check_equivalent("libhello^777")

# Non-existent crate
check_equivalent("unobtanium")

# Pinned folder
init_local_crate("yyy", enter=False)
yyy_path = os.path.join(os.getcwd(), "yyy")
check_equivalent("yyy~0", path=yyy_path)

# Prepare repository
head = init_git_repo("yyy")
branch = git_branch("yyy")
url = "git+file:" + yyy_path  # to be recognizable as a git url

# Simple git remote, explicit crate & version
check_equivalent(dep="yyy~0", url=url)

# Explicit commit
check_equivalent(dep="yyy~0", url=url, commit=head)

# Explicit branch
check_equivalent(dep="yyy~0", url=url, branch=branch)

print('SUCCESS')
