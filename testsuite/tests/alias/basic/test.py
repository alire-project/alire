"""
Basic check of alias definition in configuration
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

# Get the "hello" project and enter its directory
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])

# Define an alias locally
run_alr('config', '--set', 'alias.my_alias', 'exec echo Test an alias')

# Use the alias
p = run_alr('my_alias',
            quiet=False) # -q will hide the output of the exec command

assert_match('Test an alias', p.out, flags=re.S)

print('SUCCESS')
