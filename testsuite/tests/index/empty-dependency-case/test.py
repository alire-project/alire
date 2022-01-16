"""
Test that an empty nested table in dependencies does not cause an error.
Bugfix #906: https://github.com/alire-project/alire/pull/906
"""

from drivers.alr import run_alr, init_local_crate, alr_manifest
from drivers.asserts import assert_match

init_local_crate()

# Create the problematic table
with open(alr_manifest(), "at") as manifest:
    manifest.write("[[depends-on]]\n")
    manifest.write("[depends-on.'case(os)'.linux."
                   "'case(distribution)'.ubuntu]\n")

# The following command failed pre-bugfix, all is OK if it does not complain
p = run_alr("update")


print('SUCCESS')
