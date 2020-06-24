"""
Test that a dependency that is introduced by two dependents with different
constraints is properly solved and shown.
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Initialize project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Add dependency on hello^1. Solution is hello=1.0.1 --> libhello=1.1.0
run_alr('with', 'hello^1')
p = run_alr('with', '--solve')
assert_match('.*'  # skip solution
             'Dependencies \(graph\):\n'
             '   hello=1.0.1 --> libhello=1.1.0 \(\^1.0\).*',
             p.out, flags=re.S)

# Add dependency on superhello*. Solution is superhello=1.0 --> libhello=1.0.1
# This implies a downgrade from libhello=1.1.0 to libhello=1.0.1, which is the
# only possible combination of libhello^1.0 & libhello~1.0
run_alr('with', 'superhello')
p = run_alr('with', '--solve')
assert_match('.*'  # skip solution
             'Dependencies \(graph\):\n'
             '   hello=1.0.1      --> libhello=1.0.1 \(\^1.0\)\n'
             '   superhello=1.0.0 --> libhello=1.0.1 \(~1.0\).*',
             p.out, flags=re.S)

print('SUCCESS')
