"""
Check that updating an incomplete solution is doable resulting in no changes
"""

import re
import os

from drivers.alr import run_alr
from drivers.asserts import assert_match
from glob import glob


# Add a dependency and force it missing by pinning it to non-existing version
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
run_alr('with', 'libhello')
run_alr('pin', '--force', 'libhello=3')

# See that updating succeeds
run_alr('update')

# Check that the solution is still the expected one
p = run_alr('with', '--solve')
assert_match('.*Dependencies \(external\):\n'
             '   libhello=3.0.0.*',
             p.out, flags=re.S)


print('SUCCESS')
