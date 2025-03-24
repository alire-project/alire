"""
Test the skeleton tests crate created in `alr init`
"""

import os.path
from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match

init_local_crate(with_test=True)

p = run_alr("test")
assert_match(".*\[ PASS \] assertions_enabled.*", p.out)
# default test after init always fails

print('SUCCESS')
