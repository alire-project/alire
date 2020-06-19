"""
Detect that a given folder to pin contains an unexpected crate
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import path_separator
from glob import glob

# Retrieve a crate
run_alr('get', 'hello=1')
target = glob('hello*')[0]

# Initialize a workspace
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Add a dependency as pinned dir without metadata; this should succeed
run_alr('with', 'nothello', '--use', '..')

# Try to repin to a dir with valid crate metadata
p = run_alr('with', 'nothello', '--use', '..' + path_separator() + target,
            complain_on_error=False)

# Expected error
assert_match('.*crate mismatch: expected nothello but found hello at.*', p.out)
# skip test-specific path

print('SUCCESS')
