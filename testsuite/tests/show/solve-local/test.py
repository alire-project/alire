"""
Test that the dependencies in a local crate are properly solved
"""

import os
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

# Add a non-existent dependency

run_alr('with', 'unobtanium', force=True)

# Verify that it is properly shown as unsolvable

p = run_alr('show', '--solve', quiet=False)
assert_match(r'.*\n'
             r'Dependencies \(solution\):\n'
             r'   libhello=1\.0\.0\n'
             r'Dependencies \(missing\):\n'
             r'   unobtanium\* \(direct,missed:unknown\)\n'
             r'Dependencies \(graph\):\n'
             r'   xxx=0\.1\.0-dev --> libhello=1\.0\.0 \(\^1\.0\.0\)\n'
             r'   xxx=0\.1\.0-dev --> unobtanium\*\n'
             r'Warning: Dependencies cannot be met\n',
             p.out)


print('SUCCESS')
