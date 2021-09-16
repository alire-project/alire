"""
Verify the global --force switch is in effect
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

assert_match('.*force flag:[\s]+TRUE.*',
             run_alr('version', '--force').out)

assert_match('.*force flag:[\s]+TRUE',
             run_alr('version', '-f').out,
             flags=re.S)

print('SUCCESS')
