"""
Check detection of bad file in index
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_match

p = run_alr("crates", complain_on_error=False)
assert_match('.*ERROR: Unexpected file in index:.*shouldnt_be_here.+',
             p.out, flags=re.S)

print('SUCCESS')
