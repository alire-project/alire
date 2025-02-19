"""
Check pinning to a crate in a subdir of a remote, cleanup and redeploy
"""

import os
import shutil

from drivers.alr import run_alr, init_local_crate
from drivers.helpers import init_git_repo
from drivers.asserts import assert_eq, assert_substring


# Ensure the "remote" looks like a git repo
URL = "git+file:" + os.path.join(os.getcwd(), "upstream")


def verify(head=""):  # Either head or branch /= ""
    # Check that the linked dir exists at the expected location
    pin_path = (f"alire/cache/pins/crate" +
                ("" if head == "" else f"_{head[:8]}") + 
                "/crate")
    assert os.path.isdir(pin_path), \
        f"Expected path not found: {pin_path}\n" \
        f"Contents of alire/cache/pins: {os.listdir('alire/cache/pins')}\n"

    # Verify info reported by alr
    p = run_alr("pin")
    assert_eq(f"crate file:{pin_path} {URL}" +
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
    run_alr("with", "--del", "crate")      # Remove dependency
    p = run_alr("pin")
    assert_eq(f"There are no pins\n", p.out)  # Ensure pin is gone
    shutil.rmtree("alire")                    # Total cleanup outside of alr


# Initialize a git repo that will act as the "online" remote
os.mkdir("upstream")
os.chdir("upstream")
init_local_crate(name="crate", binary=False, enter=False)
head = init_git_repo(".")  # The repo is rooted at "upstream", the crate is at "upstream/crate"
os.chdir("..")

# Initialize a client crate that will use the remote
init_local_crate()  # This leaves us inside the new crate

# Add using with directly
run_alr("with", "--use", URL, "--commit", head, "--subdir", "crate")
verify(head)

# Add using with, without head commit
run_alr("with", "--use", URL, "--subdir", "crate")
verify()

# Pin afterwards, with commit
run_alr("with", "crate", force=True)  # force, as it is unsolvable
run_alr("pin", "crate", "--use", URL, "--commit", head, "--subdir", "crate")
verify(head)

# Pin afterwards, without commit
run_alr("with", "crate", force=True)
run_alr("pin", "crate", "--use", URL, "--subdir", "crate")
verify()

# BONUS: verify that linking to a local folder with --subdir is rejected
p = run_alr("with", "crate", "--use", "../upstream", "--subdir", "crate", 
            complain_on_error=False)
assert_substring("Pins to local directories do not accept the --subdir switch", p.out)

print('SUCCESS')
