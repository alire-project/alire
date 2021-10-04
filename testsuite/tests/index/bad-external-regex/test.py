"""
Check regex validation on index loading
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

import re

p = run_alr("index", "--check", complain_on_error=False)
assert p.status != 0, "unexpected success"
assert_match(".*invalid regular expression.*", p.out)

print('SUCCESS')
