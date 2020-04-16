"""
Verify that more than one dependency can be added
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

# Initialize a project, enter it and two dependencies

p = run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
p = run_alr('with', 'libhello')
p = run_alr('with', 'hello')
p = run_alr('show')

assert_match('.*\n'
             'Dependencies \(direct\):\n'
             '   hello\*\n'
             '   libhello\*\n',
             p.out, flags=re.S)

print('SUCCESS')
