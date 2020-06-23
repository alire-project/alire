"""
Addition of dependencies directly as a pinned directory
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import path_separator, with_project

# Initialize a workspace, enter, and add a pinned dependency
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Prepend the library we want to use to its project file
with_project('xxx.gpr', 'libhello')

# Verify it doesn't build without it
p = run_alr('build', complain_on_error=False)
assert p.status != 0, "Build should fail"

# Add pinned, check that it builds
run_alr('with', 'libhello', '--use', '../my_index/crates/libhello_1.0.0')
run_alr('build')

# Check the pin shows in the solution
p = run_alr('with', '--solve')
# For this match we don't know where the test is temporarily put, so we skip
# over some parts of the output
s = re.escape(path_separator())  # platform-dependent
assert_match('.*Dependencies \(external\):.*'
             'libhello\* \(direct,linked'
             ',pin=.*' + s + 'my_index' + s +
             'crates' + s + 'libhello_1.0.0\).*',
             p.out, flags=re.S)

# Check that removing the dependency works and build is again failing
run_alr('with', '--del', 'libhello')
p = run_alr('build', complain_on_error=False)
assert p.status != 0, "Build should fail"

print('SUCCESS')
