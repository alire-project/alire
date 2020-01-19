"""
Test unindexed external output with show/search
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

p = run_alr('show', 'hello_world', '--external')
assert_match('.*Hint.*'
             'Not yet available through the Alire project.*'
             'Must be provided by the user.*',
             p.out, flags=re.S)

p = run_alr('search', 'hello_world', '--external')
assert_match('.*hello_world.*'
             'E\ .*'     # single E means external, no detection attempted
             'external.*'
             'This is the regular short description.*'
             'Not yet available through the Alire project.*',
             p.out, flags=re.S)

p = run_alr('search', 'hello_world', '--external-detect')
assert_match('.*hello_world.*'
             'EU.*'     # EU means external, detection failed
             'external.*'
             'This is the regular short description.*'
             'Not yet available through the Alire project.*',
             p.out, flags=re.S)

print('SUCCESS')
