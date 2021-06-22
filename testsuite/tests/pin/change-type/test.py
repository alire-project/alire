"""
Change a pinned dependency from a version to a folder and back, using manifest
"""

import os
import re

from drivers.alr import run_alr, alr_pin
from drivers.asserts import assert_match
from drivers.helpers import dir_separator


def check_version_pin():
    p = run_alr('show', '--solve')
    assert_match('.*Dependencies \(solution\):'
                 '.*libhello=1.0.0.*',
                 p.out, flags=re.S)


# Initialize a workspace, enter, and add a regular dependency
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
run_alr('with', 'libhello^1')

# Pin to a version
alr_pin('libhello', version='1.0')

# Check that it shows as such in the solution
check_version_pin()

# Repin to a folder
alr_pin('libhello', path='../crates/libhello_1.0.0')

# Check that it shows as such in the solution
p = run_alr('show', '--solve')
s = re.escape(dir_separator())  # platform-dependent
assert_match('.*Dependencies \(external\):.*'
             'libhello\^1 \(direct,linked'
             ',pin=..' + s +  # relative link should be preserved
             'crates' + s + 'libhello_1.0.0\).*',
             p.out, flags=re.S)

# Repin to a version and check again
alr_pin('libhello', version='1.0')
check_version_pin()


print('SUCCESS')
