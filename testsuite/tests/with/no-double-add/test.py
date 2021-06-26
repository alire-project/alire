"""
Verify prevention of double-add of dependencies
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq

# Initialize a project, enter it and double-add a dependency

p = run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
p = run_alr('with', 'libhello')
p = run_alr('with', 'libhello', quiet=False, complain_on_error=False)

assert_eq(
    'ERROR: libhello is already a direct dependency.\n',
    p.out)

print('SUCCESS')
