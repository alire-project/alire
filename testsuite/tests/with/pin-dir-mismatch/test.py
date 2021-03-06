"""
Detect that a given folder to pin contains an unexpected crate
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import dir_separator
from glob import glob

# Retrieve a crate
run_alr('get', 'hello=1')
target = glob('hello*')[0]

# Initialize a workspace
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Try to pin the hello crate as local dir dependency
p = run_alr('with', 'nothello', '--use', '..' + dir_separator() + target,
            complain_on_error=False)

# Expected error
assert_match('.*expected nothello but found hello.*', p.out)
# skip test-specific path

print('SUCCESS')
