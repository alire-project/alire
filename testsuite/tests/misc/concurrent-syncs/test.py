"""
Several workspaces syncing the same shared build folder at the same time all
succeed, and leave a complete build folder
"""

import os
import subprocess

from drivers import builds
from drivers.alr import alr_with, init_local_crate
from drivers.asserts import assert_eq

COUNT = 20
# No guarantee of triggering an error, of course, but it happens most of the
# time in local runs without locking.

# Workspaces with the same dependency, and so the same build folder for it

for i in range(COUNT):
    init_local_crate(f"ws{i}")
    alr_with("libhello")
    builds.sync()
    os.chdir("..")

# Remove the build folder they share, so all of them sync it again at once
# from the vault

builds.clear_builds_dir()

procs = [subprocess.Popen([os.environ["ALR_PATH"], "-n", "build",
                           "--stop-after=sync"],
                          cwd=f"ws{i}",
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                          text=True)
         for i in range(COUNT)]

for i, p in enumerate(procs):
    out, _ = p.communicate(timeout=120)
    assert_eq(0, p.returncode, f"ws{i} failed:\n{out}")

# The build folder is complete

assert os.path.isfile(
    builds.complete_copy_flag(builds.find_dir("libhello"))), \
    "Incomplete build folder"

print("SUCCESS")
