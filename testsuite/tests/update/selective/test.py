"""
Test selective crate update
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on hello1 and hello2
run_alr('with', 'hello1>0', 'hello2>0')

# Verify solution depends on 0.1 versions
p = run_alr('with', '--solve')
assert_match('.*Dependencies \(solution\):\n'
             '   hello1=0\.1.*\n'
             '   hello2=0\.1.*\n',
             p.out, flags=re.S)

# Add a new index that contains updates
run_alr('index', '--name', 'updates', '--add', '../my_index/updated')

# Do selective update and check than only the requested crate has been updated
run_alr('update', 'hello2')
p = run_alr('with', '--solve')
assert_match('.*Dependencies \(solution\):\n'
             '   hello1=0\.1.*\n'
             '   hello2=0\.2.*\n',
             p.out, flags=re.S)

print('SUCCESS')
