"""
Change a pinned dependency from a version to a folder and back
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import path_separator


def check_version_pin():
    p = run_alr('show', '--solve')
    assert_match('.*Dependencies \(solution\):'
                 '.*libhello=1.0.0.*',
                 p.out, flags=re.S)


# Initialize a workspace, enter, and add a regular dependency
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
run_alr('with', 'libhello')

# Pin to a version
p = run_alr('pin', 'libhello=1.0')

# Check that it shows as such in the solution
check_version_pin()

# Repin to a folder
run_alr('pin', 'libhello', '--use', '../my_index/crates/libhello_1.0.0')

# Check that it shows as such in the solution
p = run_alr('show', '--solve')
s = re.escape(path_separator())  # platform-dependent
assert_match('.*Dependencies \(external\):.*'
             'libhello\* \(direct,linked'
             ',pin=.*' + s + 'my_index' + s +
             'crates' + s + 'libhello_1.0.0\).*',
             p.out, flags=re.S)

# Repin to a version and check again
p = run_alr('pin', 'libhello=1.0')
check_version_pin()


print('SUCCESS')
