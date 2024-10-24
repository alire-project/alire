"""
Verify that using manual edition and `alr pin` result in equivalent outputs
"""

import os

from e3.fs import rm

from drivers.alr import run_alr, alr_pin, init_local_crate
from drivers.asserts import assert_eq
from drivers.helpers import init_git_repo, git_branch


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

        # Cleanup (use e3.fs, because shutil.rmtree() fails on links on Windows)
        os.chdir("..")
        rm("xxx", recursive=True)


# Local pinnable crate
init_local_crate("yyy", enter=False)
yyy_path = os.path.join(os.getcwd(), "yyy")

# Local pinnable raw project
os.mkdir("zzz")
zzz_path = os.path.join(os.getcwd(), "zzz")

# Simple pin, no restrictions
check_equivalent("yyy", path=yyy_path)
check_equivalent("zzz", path=zzz_path)

# Prepare repository
head = init_git_repo("yyy")
branch = git_branch("yyy")
url = "git+file:" + yyy_path  # to be recognizable as a git url

# Simple git remote, explicit crate
check_equivalent(crate="yyy", url=url)

# Explicit commit
check_equivalent(crate="yyy", url=url, commit=head)

# Explicit branch
check_equivalent(crate="yyy", url=url, branch=branch)

print('SUCCESS')
