"""
Test that a pin to a lower version downgrades and retrieves the new version
"""

import os

from drivers.alr import run_alr, alr_pin
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import check_line_in

import os
import re


# Verify that proper version of libchild is in the printed and disk solution
def check_child(version, output, pinned):
    # Verify output
    assert_match('.*\n'
                 'Dependencies \(solution\):\n'
                 '   libchild=' + version +
                 (" \(pinned\)" if pinned else "") + '.*\n',
                 output, flags=re.S)

    # Verify lockfile
    check_line_in('alire.lock', 'name = "libchild"')
    check_line_in('alire.lock', f'version = "{version}"')

    # Verify dependency folders
    assert os.path.exists('alire/cache/dependencies/libchild_' + version +
                          '_filesystem')


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on child (there are 0.1 and 0.2, so 0.2 used initially)
run_alr('with', 'libchild>0')
p = run_alr('show', '--solve')
check_child('0.2.0', p.out, pinned=False)

# Pin it to a downgrade
alr_pin('libchild', version='0.1')

# Verify new version
p = run_alr('show', '--solve')
check_child('0.1.0', p.out, pinned=True)


print('SUCCESS')
