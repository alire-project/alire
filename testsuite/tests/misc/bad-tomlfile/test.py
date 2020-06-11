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

with open("alire/xxx.toml", "a") as myfile:
    myfile.write("SHOULND'T BE HERE")

# Verify that the expected warning is given
p = run_alr('show', complain_on_error=False, quiet=False)  # Let warn through

assert_match('.*Could not load crate information from.*'
             'If this workspace was created with a'
             ' previous alr version you may need to recreate it.*',
             p.out, flags=re.S)

print('SUCCESS')
