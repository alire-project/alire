"""
Test that an unavailable native is reported during get as an installation hint
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

p = run_alr('get', 'libhello=0.9-test_unav_native',
            complain_on_error=True, quiet=False)

assert_match('Warning: The following native dependencies are unavailable within Alire:\n'
             'Warning:    make\*\n'
             'Warning: They should be made available in the environment by the user.\n',
             p.out, flags=re.S)

print('SUCCESS')
