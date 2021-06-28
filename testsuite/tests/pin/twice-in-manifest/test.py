"""
Detect a crate with two pins in the manifest
"""

import os
import re

from drivers.alr import run_alr, alr_pin
from drivers.asserts import assert_match
from drivers.helpers import dir_separator


# Initialize a workspace, enter, and add a regular dependency
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
run_alr('with', 'libhello^1')

# Pin to a version
alr_pin('libhello', version='1.0')

# Manually force a second pin
with open("alire.toml", "at") as manifest:
    manifest.write("[[pins]]\n")
    manifest.write("libhello = { path = '.' }\n")

# Check that the second pin is rejected
p = run_alr('pin', complain_on_error=False)
assert_match('.*pin for crate libhello is specified more than once.*',
             p.out)


print('SUCCESS')
