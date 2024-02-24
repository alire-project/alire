"""
Test that hinted native dependencies appear correctly in 'alr show'.
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

p = run_alr('show', 'libhello=0.9-test-unav-native', '--solve',
            complain_on_error=True)

assert_match('.*'
             'Dependencies \(missing\):\n'
             '   make\*'
             '.*',
             p.out, flags=re.S)

print('SUCCESS')
