"""
Test proper integrity check of a git origin
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

# Get the release and check that verification took place. This, coupled
# with not getting an error, means that the release verified properly:
p = run_alr('-v', 'get', 'libhello=1.0.0-tarball',
            complain_on_error=True, quiet=False)
assert_match('.*Verifying integrity.*', p.out, flags=re.S)

print('SUCCESS')
