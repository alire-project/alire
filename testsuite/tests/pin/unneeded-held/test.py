"""
Test that removing a pinned dependency keeps the pinned release in the solution
"""

import os

from drivers.alr import run_alr, alr_pin
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import check_line_in

import os
import re


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on a couple of crates (simplifies checking the solution later)
# libparent depends on libchild
run_alr('with', 'libparent')

# Pin the child
alr_pin('libchild', version='0.2')

# Remove parent
run_alr('with', '--del', 'libparent')

# Check pin is there
p = run_alr('pin')
assert_eq('libchild 0.2.0\n',
          p.out)

# Check that there are no dependencies
p = run_alr('with')
assert_eq('Dependencies (direct):\n'
          '   (empty)\n',
          p.out)

# But the pinned release is still in the solution
p = run_alr('with', '--solve')
assert_match('.*'
             'Dependencies \(solution\):\n'
             '   libchild=0\.2\.0 \(pinned\) \(origin: filesystem\)\n'
             'Dependencies \(graph\):.*',
             p.out, flags=re.S)


print('SUCCESS')
