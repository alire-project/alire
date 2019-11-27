"""
Check that `alr show` shows the same info when used outside/inside working copy
"""

import os

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_eq

# Outside run
run1 = run_alr('show', 'libhello')

run_alr('get', 'libhello')
os.chdir(glob('libhello*')[0])

# Inside run
run2 = run_alr('show')

assert_eq(run1.out, run2.out)

print('SUCCESS')
