"""
Detect that a given folder to pin contains a crate and use its info
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import dir_separator
from glob import glob

# Retrieve a crate
run_alr('get', 'hello=1')
target = glob('hello*')[0]

# Initialize a workspace
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Pin the hello crate as local dir dependency
run_alr('with', 'hello', '--use', '..' + dir_separator() + target)

# Verify that hello dependencies are detected and used
p = run_alr('with', '--solve')
assert_match('''.*Dependencies \(solution\):
   hello=1\.0\.0 .*
   libhello=1\.1\.0 .*''',  # we skip non-relevant details
             p.out, flags=re.S)

print('SUCCESS')
