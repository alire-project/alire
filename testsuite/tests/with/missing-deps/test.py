"""
Check that `with` works with missing dependencies
"""

import re
import os

from drivers.alr import run_alr
from drivers.asserts import assert_match
from glob import glob


# Initialize test crate
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# 1st test, adding an entirely inexistent crate
run_alr('with', 'unobtanium', '--force')

# 2nd test, adding a dependency that exists but with missing version
run_alr('with', 'libhello^3', '--force')

# 3rd test, adding a dependency that has missing dependencies
run_alr('with', 'hello^3', '--force')

# Check that the solution contains the requested dependencies
p = run_alr('with', '--solve')
assert_match('.*Dependencies \(solution\):\n'
             '   hello=3\.0\.0.*'  # skip origin
             'Dependencies \(external\):\n'
             '   libhello\^3.*'    # skip flags
             '   unobtanium\*.*',
             p.out, flags=re.S)


print('SUCCESS')
