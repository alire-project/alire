"""
Ensure that no unnecessary rebuilds happend due to crate config generation
"""

import os
from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_match
from drivers.helpers import prepend_to_file

init_local_crate()
run_alr("build") # First build

# Same build, nothing should be recompiled
p = run_alr("build", quiet=False)
assert_match('.*gprbuild: "xxx" up to date', p.out)

# Switch to another profile and build must happen
p = run_alr("build", "--validation", quiet=False)
assert_match('.*\[link\]         xxx.adb', p.out)

# Use same profile, nothing should be recompiled
p = run_alr("build", "--validation", quiet=False)
assert_match('.*gprbuild: "xxx" up to date', p.out)

print('SUCCESS')
