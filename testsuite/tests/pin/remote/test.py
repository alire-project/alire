"""
Check pinning to a remote, cleanup and redeploy
"""

import os
import shutil

from drivers.alr import run_alr, init_local_crate
from drivers.helpers import init_git_repo, git_branch
from drivers.asserts import assert_eq

s = os.sep


def verify(head=""):  # Either head or branch /= ""
    # Check that the linked dir exists at the expected location
    pin_path = (f"alire{s}cache{s}pins{s}upstream" +
                ("" if head == "" else f"_{head[:8]}"))
    assert os.path.isdir(pin_path)

    # Verify info reported by alr
    p = run_alr("pin")
    assert_eq(f"upstream file:{pin_path} ../upstream.git" +
              ("" if head == "" else f"#{head[0:8]}") + "\n",
              p.out)

    # Verify building with pinned dependency
    run_alr("build")

    # Verify removal of cached download
    run_alr("clean", "--cache")
    assert not os.path.isdir(pin_path)

    # Verify automatic redownload when needed
    run_alr("build")

    # Prepare for next test
    run_alr("with", "--del", "upstream")      # Remove dependency
    p = run_alr("pin")
    assert_eq(f"There are no pins\n", p.out)  # Ensure pin is gone
    shutil.rmtree("alire")                    # Total cleanup outside of alr


# Initialize a git repo that will act as the "online" remote
init_local_crate(name="upstream", binary=False)
head = init_git_repo(".")
os.chdir("..")
os.rename("upstream", "upstream.git")  # so it is recognized as git repo

# Initialize a client crate that will use the remote
init_local_crate()  # This leaves us inside the new crate

# Add using with directly
run_alr("with", "--use", "../upstream.git", "--commit", head)
verify(head)

# Add using with, without head commit
run_alr("with", "--use", "../upstream.git")
verify()

# Pin afterwards, with commit
run_alr("with", "upstream", force=True)  # force, as it is unsolvable
run_alr("pin", "upstream", "--use", "../upstream.git", "--commit", head)
verify(head)

# Pin afterwards, without commit
run_alr("with", "upstream", force=True)
run_alr("pin", "upstream", "--use", "../upstream.git")
verify()

print('SUCCESS')
