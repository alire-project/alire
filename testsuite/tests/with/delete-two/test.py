"""
Check that we can delete dependencies from an array with several entries
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import content_of

manifest = "alire.toml"

run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Manually add two dependencies to the same array entry
with open(manifest, 'a') as file:
    file.write('[[depends-on]]\n'
               'libhello = "*"\n'
               'hello = "*"')

# Check removal in 1st-2nd order
run_alr('with', '--del', 'libhello')
assert 'libhello = "*"' not in content_of(manifest)  # Dep gone
assert 'hello = "*"' in content_of(manifest)         # Dep stays

run_alr('with', '--del', 'hello')
assert 'hello = "*"' not in content_of(manifest)     # No trace of deps anymore
assert '[[depends-on]]' not in content_of(manifest)  # No trace of deps anymore

# Same tests in reverse order of dependency

with open(manifest, 'a') as file:
    file.write('[[depends-on]]\n'
               'hello = "*"\n'
               'libhello = "*"')

# Check removal in 2nd-1st order
run_alr('with', '--del', 'libhello')
assert 'libhello = "*"' not in content_of(manifest)  # Dep gone
assert 'hello = "*"' in content_of(manifest)         # Dep stays

run_alr('with', '--del', 'hello')
assert 'hello = "*"' not in content_of(manifest)     # No trace of deps anymore
assert '[[depends-on]]' not in content_of(manifest)  # No trace of deps anymore


print('SUCCESS')
