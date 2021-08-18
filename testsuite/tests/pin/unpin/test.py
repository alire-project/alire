"""
Test unpinning
"""

import os

from drivers.alr import run_alr, alr_pin, alr_unpin, alr_lockfile
from drivers.asserts import assert_eq
from drivers.helpers import check_line_in


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on libhello
run_alr('with', 'libhello')

# Pin the version of libhello and verify pin is there
alr_pin('libhello', version="1")
p = run_alr('pin')
assert_eq('libhello 1.0.0\n', p.out)

# Update and verify that the pin has survived
run_alr('update')
p = run_alr('pin')
assert_eq('libhello 1.0.0\n', p.out)

# Delete lockfile and verify the pin has survived
os.remove(alr_lockfile())
p = run_alr('pin')
assert_eq('libhello 1.0.0\n', p.out)

# Unpin and verify pin is not there
alr_unpin('libhello')
p = run_alr('pin')
assert_eq('There are no pins\n', p.out)


print('SUCCESS')
