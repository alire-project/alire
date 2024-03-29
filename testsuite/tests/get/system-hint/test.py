"""
Test that an unavailable system is reported during get as an installation hint
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

p = run_alr('get', 'libhello=0.9-test-unav-native', force=True,
            complain_on_error=True, quiet=False)

assert_match('.*'   # Skip user interaction and solution changes
             'Warning: The following external dependencies'
             ' are unavailable within Alire:\n'
             'Warning:    make\*\n'
             'Warning: They should be made available in the'
             ' environment by the user.\n'
             '.*',  # Skip final solution summary
             p.out, flags=re.S)

print('SUCCESS')
