"""
Test pin/unpin with --all
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on hello1 and hello2
run_alr('with', 'hello1')
run_alr('with', 'hello2')

# Pin and check
run_alr('pin', '--all')
p = run_alr('pin')
assert_eq('hello1 0.1.0\n'
          'hello2 0.1.0\n',
          p.out)

# Unpin and check
run_alr('pin', '--unpin', '--all')
p = run_alr('pin')
assert_eq('There are no pins\n', p.out)

print('SUCCESS')
