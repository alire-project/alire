"""
Check detection of manifest in wrong parent
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_match

p = run_alr("list", complain_on_error=False)
assert_match('.*ERROR: Mismatch between manifest and parent:.*',
             p.out, flags=re.S)

print('SUCCESS')
