"""
Retrieve a crate with a partial solution available
"""

import re
import os

from drivers.alr import run_alr
from drivers.asserts import assert_match
from glob import glob


# Get the "hello" crate with version 3, which has missing libhello=3 dep
run_alr('get', 'hello=3', force=True)
os.chdir(glob('hello*')[0])

# Check missing dependency is properly identified
p = run_alr('with', '--solve')
assert_match('.*Dependencies \(missing\):\n'
             '   libhello\^3\.0.*',
             p.out, flags=re.S)

# Double-check that build fails
p = run_alr('build', complain_on_error=False)
assert p.status != 0, "Build should have failed"


print('SUCCESS')
