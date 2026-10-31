"""
Under update.manually_only, missing dependency sources are still redeployed,
as the automatic sync keeps existing releases instead of being skipped.
"""

import os
from shutil import rmtree

from drivers.alr import alr_settings_set, init_local_crate, run_alr

init_local_crate()
run_alr("with", "hello")
alr_settings_set("update.manually_only", "true")

target = os.path.join("alire", "cache", "dependencies",
                      "hello_1.0.1_filesystem")
assert os.path.isdir(target), "Dependency missing after its deployment"

rmtree(target)
run_alr("with")  # Any command in the workspace
assert os.path.isdir(target), "Dependency not redeployed"

print("SUCCESS")
