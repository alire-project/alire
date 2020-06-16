"""
Test that `alr with --url` without explicit crate autodetects the target crate
and correctly adds the pinned dependency
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import path_separator
from glob import glob

# Retrieve a crate
run_alr('get', 'hello=1')
target = glob('hello*')[0]

# Initialize a workspace
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Add the directory as pinned crate, with autodetection (no crate given,
# inferred from the crate metadata found at target).
run_alr('with', '--url', '..' + path_separator() + target)

# Verify that hello^1 is a new dependency and also that hello dependencies are
# in the solution.
p = run_alr('with', '--solve')
assert_match('''Dependencies \(direct\):
   hello\^1\.0\.0
.*Dependencies \(solution\):
   hello=1\.0\.0 .*
   libhello=1\.1\.0 .*''',  # we skip non-relevant details
             p.out, flags=re.S)

print('SUCCESS')
