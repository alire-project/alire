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
             'expected \[sha512:deadbeef\] but got \[sha512:'
             '76ac929d1346bff4ae77f92008a18578383c80c920850cc4d45877996fd5cbda'
             '3ba73096697def23a6c6d848d509724bbe7351e750a72d4e77fd215789c8f64b'
             '\].*',
             p.out, flags=re.S)

print('SUCCESS')
