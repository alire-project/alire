"""
Test proper loading of environment properties
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re


# With conditionals
p = run_alr('show', 'hello')

assert_match('.*'
             '      when Linux => \(Environment: CONDVAR=uvw\)\n'
             '.*'
             '   Environment: VAR1=\$VAR1:abc\n'
             '   Environment: VAR2=xyz:\$VAR2\n'
             '   Environment: VAR3=pqr\n'
             '.*',
             p.out, flags=re.S)

# Check resolved conditional
p = run_alr('show', 'hello', '--system')

assert_match('.*'
             '   Environment: CONDVAR=uvw\n'
             '.*',
             p.out, flags=re.S)

print('SUCCESS')
