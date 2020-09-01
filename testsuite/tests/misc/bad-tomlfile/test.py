"""
Check that a bad crate file is warned about
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import os
import re

# Create a new crate
run_alr('init', '--bin', 'xxx')

# And muck its tomlfile
os.chdir('xxx')

with open("alire.toml", "a") as myfile:
    myfile.write("SHOULND'T BE HERE")

# Verify that the expected error is given
p = run_alr('show', complain_on_error=False)

assert_match('.*Cannot continue with invalid session.*'
             'Failed to load.*',
             p.out, flags=re.S)

print('SUCCESS')
