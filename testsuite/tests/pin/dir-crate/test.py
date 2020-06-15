"""
Detect that a given folder to pin contains a crate and use its info
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

# Add dependency as regular crate; this has missing dependencies so we have to
# force. This brings in hello=4.
run_alr('with', 'hello', '--force')

# Pin the hello crate as local dir dependency. The version in the folder is
# different to the one we had in the solution, so this should cause a downgrade
# but with complete solution. Now hello=1 --> libhello=1.1.
run_alr('pin', 'hello', '--url', '..' + path_separator() + target)

# Verify that hello dependencies are detected and used, and are the ones
# corresponding to the linked dir versions.
p = run_alr('with', '--solve')
assert_match('''.*Dependencies \(solution\):
   hello=1\.0\.0 .*
   libhello=1\.1\.0 .*''',  # we skip non-relevant details
             p.out, flags=re.S)

print('SUCCESS')
