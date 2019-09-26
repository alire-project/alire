"""
Test that a mismatching hash in a git origin is properly reported
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

# Get a release with different hash and check the error:
p = run_alr('get', 'libhello=1.0.0-gitbad', complain_on_error=False)
assert_match('ERROR: release integrity test failed: '
             'expected \[sha512:deadbeef\] but got '
             '\[sha512:f9d5a85fec4db46d5a2859057658c01ee4fe1ab412dab'
             '80bcbb426102b5dcb147ab7e0744e781d045e5d2b50ec3cb4f0990'
             'c8ce66e52cda37c88833d0f814500\].*',
             p.out, flags=re.S)

print('SUCCESS')
