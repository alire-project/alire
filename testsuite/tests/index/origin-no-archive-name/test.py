"""
Test that origins lacking archive names in indexes are properly reported.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

p = run_alr('show', 'hello_world', complain_on_error=False, quiet=False)
assert_match('.*unable to determine archive name from URL.*',
             p.out, flags=re.S)

print('SUCCESS')
