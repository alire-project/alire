"""
Replacing a dependency with a pinned folder
"""

import os
import re

from drivers.alr import run_alr, alr_pin, alr_unpin
from drivers.asserts import assert_match
from drivers.helpers import dir_separator, with_project

# Initialize a workspace, enter, and add a regular dependency
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Prepend the library we want to use to its project file
with_project('xxx.gpr', 'libhello')

# Verify it doesn't build without it
p = run_alr('build', complain_on_error=False)
assert p.status != 0, "Build should fail"

# Add normally and then pin, check that it builds
run_alr('with', 'libhello')
alr_pin('libhello', path='../crates/libhello_1.0.0')
run_alr('build')

# Check the pin shows in the solution
p = run_alr('with', '--solve')
# For this match we don't know where the test is temporarily put, so we skip
# over some parts of the output
assert_match('.*Dependencies \(solution\):.*'
             'libhello\^1.0.0 \(direct,linked'
             ',path=../crates/libhello_1.0.0\).*',  # relative, always fwd slash
             p.out, flags=re.S)

# Check that unpinning the dependency works and now the dependency is show
# as a regular one from the index
alr_unpin('libhello')
p = run_alr('show', '--solve')
assert_match('.*Dependencies \(solution\):'
             '.*libhello=1.0.0.*',
             p.out, flags=re.S)

print('SUCCESS')
