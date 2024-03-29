"""
Test that a pinned release does not get updated
"""

import os

from drivers.alr import run_alr, alr_pin, alr_unpin, alr_lockfile
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import check_line_in

import re


# Verify that proper version of libchild is in the printed and disk solution
def check_child(version, output, pinned):
    # Verify output
    assert_match('.*\n'
                 'Dependencies \(solution\):\n'
                 '   libchild=' + version +
                 (' \(pinned\)' if pinned else "") + '\n'
                 '   libparent=1\.0\.0\n'
                 '.*\n',
                 output, flags=re.S)

    # Verify lockfile
    check_line_in(alr_lockfile(), 'name = "libchild"')
    check_line_in(alr_lockfile(), f'version = "{version}"')


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on child=0.1 (there is also 0.2)
run_alr('with', 'libchild=0.1')

# Pin it
alr_pin('libchild', version="0.1")

# To avoid pinning and downgrading (that's a different test), we depend on
# a crate that also depends on libchild. This way we can remove the exact
# libchild dependency and verify the pin holds
run_alr('with', 'libparent')

# Remove child dependency
run_alr('with', '--del', 'libchild')

# But keeping the pin (alr with --del will remove also the pin)
alr_pin('libchild', version="0.1")

# Verify pinned version is still in the solution, pre-update:
p = run_alr('show', '--solve')
check_child('0.1.0', p.out, pinned=True)

# Run an update and verify solution is still the same
run_alr('update')
p = run_alr('show', '--solve')
check_child('0.1.0', p.out, pinned=True)

# Unpin and check upgraded solution
alr_unpin('libchild')
p = run_alr('show', '--solve')
check_child('0.2.0', p.out, pinned=False)


print('SUCCESS')
