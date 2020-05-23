"""
Verify that withing a dependency on a crate with only unavailable externals
works
"""

from glob import glob
import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match


# Initialize a new project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Add a dependency on 'make', defined in the index as only a hint
run_alr('with', 'make')

# Verify that it appears in the solution as unavailable external
p = run_alr('with', '--solve')
assert_match('Dependencies \(direct\):\n'
             '   make\*\n'
             'Dependencies \(external\):\n'
             '   make\*\n'
             '.*',  # Skip graph
             p.out, flags=re.S)


print('SUCCESS')
