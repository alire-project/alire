"""
Verify that using manual edition and `alr pin` result in equivalent outputs
"""

from drivers.alr import run_alr, alr_pin, init_local_crate
from drivers.asserts import assert_eq
from drivers.helpers import init_git_repo, git_branch

import os
import shutil


def check_equivalent(crate, path="", url="", commit="", branch=""):
    """
    Run manual and auto and compare outputs
    """
    manual = [False, True]
    p = [None, None]

    for i in [0, 1]:
        init_local_crate()

        # run command
        alr_pin(crate=crate, path=path, url=url,
                commit=commit, branch=branch,
                manual=manual[i])

        # get pins output
        p[i] = run_alr("pin").out

        if i == 1:
            assert_eq(p[0], p[1])

        # Cleanup
        os.chdir("..")
        shutil.rmtree("xxx")


# Local pinnable crate
init_local_crate("yyy", enter=False)
yyy_path = "../yyy"

# Local pinnable raw project
os.mkdir("zzz")
zzz_path = "../zzz"

# Simple pin, no restrictions
check_equivalent("yyy", path=yyy_path)
check_equivalent("zzz", path=zzz_path)

# Prepare repository
head = init_git_repo("yyy")
branch = git_branch("yyy")
os.rename("yyy", "yyy.git")  # to be recognizable as a git url
url = "../yyy.git"

# Simple git remote, explicit crate
check_equivalent(crate="yyy", url=url)

# Explicit commit
check_equivalent(crate="yyy", url=url, commit=head)

# Explicit branch
check_equivalent(crate="yyy", url=url, branch=branch)

print('SUCCESS')
