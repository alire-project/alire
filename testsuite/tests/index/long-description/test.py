"""
Test that long descriptions are accepted
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

p = run_alr('show', 'hello_world')
assert_match('.*Long_Description: This is the long crate description.\n'
             '\n'
             'This can be a multiline string, and TOML parsers are free to use'
             ' the platform end-of-line character during loading.*',
             p.out, flags=re.S)

print('SUCCESS')
