"""
Check detection of manifest in wrong shelf
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_match

p = run_alr("crates", complain_on_error=False)
assert_match('.*ERROR: Mismatch between manifest and shelf:.*',
             p.out, flags=re.S)

print('SUCCESS')
