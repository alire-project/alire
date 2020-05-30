"""
Test proper working of alr get --only and other follow-up commands in such an
invalid solution state
"""

from glob import glob
import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match


# Get the "hello" project and enter its directory, without solving dependencies
run_alr('get', 'hello', '--only')
os.chdir(glob('hello*')[0])

# Verify that it has no solution
p = run_alr('with', '--solve')
assert_eq('Dependencies (direct):\n'
          '   libhello^1.0\n'
          'Dependencies (solution):\n'
          '   No solving attempted\n',
          p.out)

# Verify that it has no pins
p = run_alr('pin')
assert_eq('There are no pins\n', p.out)

# Verify that updating it fixes the solution
run_alr('update')
p = run_alr('with', '--solve')
assert_match('.*\n'   # Skip dependencies
             'Dependencies \(solution\):\n'
             '   libhello=1\.0\.0.*\n'
             '.*',  # Skip graph
             p.out, flags=re.S)


print('SUCCESS')
