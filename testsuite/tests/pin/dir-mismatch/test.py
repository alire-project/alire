"""
Detect that a given folder to pin contains an unexpected crate
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import dir_separator
from glob import glob

# Prepare an empty dir
os.mkdir("nothello")

# Retrieve a crate
run_alr('get', 'hello=1')
target = glob('hello*')[0]

# Initialize a workspace
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Add a dependency as pinned dir without metadata; this should succeed
run_alr('with', 'nothello', '--use', '..')

# Detect a repin is rejected unless forced
p = run_alr('with', 'nothello', '--use', '..' + dir_separator() + target,
            complain_on_error=False)
assert_match(".*nothello is already pinned with pin .*", p.out)

# Try to repin to a dir with valid crate metadata
p = run_alr('with', 'nothello', '--use', '..' + dir_separator() + target,
            complain_on_error=False, force=True)

# Expected error
assert_match('.*expected nothello but found hello.*', p.out)
# skip test-specific path

print('SUCCESS')
