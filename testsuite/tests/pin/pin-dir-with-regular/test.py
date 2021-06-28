"""
Test that pinning another crate doesn't affect a regularly solved one
"""

import os
import re

from drivers.alr import run_alr, alr_pin
from drivers.asserts import assert_match

# Initialize a workspace, enter, and add a regular dependency
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Add a regular solvable dependency
run_alr('with', 'libhello')

# Add a missing crate
run_alr('with', 'unobtanium', '--force')

# Pin the missing crate
alr_pin('unobtanium', path='/')

# Check the solution shows both pinned dir and regular dependency
p = run_alr('with', '--solve')
# For this match we don't know where the test is temporarily put, so we skip
# over some parts of the output
assert_match('.*Dependencies \(solution\):.*'
             'libhello=1\.0\.0.*'
             'Dependencies \(external\):.*'
             'unobtanium\* \(direct,linked,.*',
             p.out, flags=re.S)

print('SUCCESS')
