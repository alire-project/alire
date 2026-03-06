"""
Verify that `alr clean` succeeds even when the config dir has been removed
before the clean. The config dir should be regenerated as part of the clean.
"""

import os
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_file_exists

init_local_crate()

# Build the crate so the config dir is generated
run_alr("build")

assert_file_exists("config")

# Remove the config dir to simulate the situation where it was deleted
shutil.rmtree("config")
assert_file_exists("config", wanted=False)

# alr clean should regenerate the config dir and succeed
run_alr("clean")

# The config dir should have been recreated
assert_file_exists("config")

print('SUCCESS')
