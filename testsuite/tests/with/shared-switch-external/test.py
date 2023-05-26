"""
Check that selective sharing doesn't affect an external crate
"""

import os
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match, match_deploy_dir

# Add an external crate, requesting it to be shared
init_local_crate()
run_alr("with", "--shared", "gnat_external")

# Ensure that the shared flag is not in the solution
p = run_alr("with", "--solve")
assert_match(".*gnat_external=[^\\n]* \(origin: external\)", p.out)

print('SUCCESS')
