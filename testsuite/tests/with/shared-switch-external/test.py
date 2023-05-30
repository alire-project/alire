"""
Check that selective sharing doesn't affect an external crate, which is always
implicitly shared.
"""

import os
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match, match_deploy_dir

# Add and external crate, without requesting sharing
init_local_crate()
run_alr("with", "gnat_external")

# Check that the shared flag is in the solution nonetheless
p = run_alr("with", "--solve")
assert_match(".*gnat_external=[^\\n]* \(shared\) \(origin: external\)", p.out)


# Start afresh in new crate
init_local_crate()

# Add an external crate, requesting it not to be shared
run_alr("with", "--shared=no_local", "gnat_external")

# Check that the shared flag is in the solution nonetheless
p = run_alr("with", "--solve")
assert_match(".*gnat_external=[^\\n]* \(shared\) \(origin: external\)", p.out)


print('SUCCESS')
