"""
Test that the dependencies in a local crate are properly solved
"""

import os.path
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Initialize a new workspace

run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Add a dependency

run_alr('with', 'libhello')

# Verify that it is properly solved and shown

p = run_alr('show', '--solve')
assert_match('.*\n'
             'Dependencies \(solution\):\n'
             '   libhello=1\.0\.0\n'
             '.*\n',
             p.out, flags=re.S)


print('SUCCESS')
