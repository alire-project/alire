"""
Test that a pinned release does not get updated
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import check_line_in

import re


# Verify that proper version of libchild is in the printed and disk solution
def check_child(version, output, pinned):
    # Verify output
    assert_match('.*\n'
                 'Dependencies \(solution\):\n'
                 '   libchild=' + version + (' \(pinned\)' if pinned else "") + '\n'
                 '   libparent=1\.0\.0\n'
                 '.*\n',
                 output, flags=re.S)

    # Verify lockfile
    check_line_in('alire/xxx.lock',
                  '[state.release.libchild."' + version + '"]')


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on child=0.1 (there is also 0.2)
run_alr('with', 'libchild=0.1')

# Pin it
run_alr('pin', 'libchild')

# To avoid pinning and downgrading (that's a different test), we depend on
# a crate that indirectly depends on libchild. This way we can remove the exact
# libchild dependency and verify the pin holds
run_alr('with', 'libparent')

# Remove child dependency
run_alr('with', '--del', 'libchild')

# Verify pinned version is still in the solution, pre-update:
p = run_alr('show', '--solve')
check_child('0.1.0', p.out, pinned=True)

# Run an update and verify solution is still the same
run_alr('update')
p = run_alr('show', '--solve')
check_child('0.1.0', p.out, pinned=True)

# Unpin and check upgraded solution
run_alr('pin', '--unpin', 'libchild')
p = run_alr('show', '--solve')
check_child('0.2.0', p.out, pinned=False)


print('SUCCESS')
