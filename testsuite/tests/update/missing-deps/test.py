"""
Check that updating an incomplete solution is doable resulting in no changes.
This is labeled manual because the pin is added through the manifest.
"""

import re
import os

from drivers.alr import run_alr, alr_pin
from drivers.asserts import assert_match
from glob import glob


# Add a dependency and force it missing by pinning it to non-existing version
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
run_alr('with', 'libhello')
alr_pin('libhello', version="3")

# See that updating succeeds
run_alr('update')

# Check that the solution is still the expected one, and also that the original
# dependency is included in the restrictions
p = run_alr('with', '--solve')
assert_match(
    '.*Dependencies \(external\):\n'
    '   ' +
    re.escape('libhello(=3.0.0) & (^2.0.0) '
              '(direct,missed:conflict,pin=3.0.0)') + '.*',
    p.out, flags=re.S)


print('SUCCESS')
