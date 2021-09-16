"""
Check that updating a manifest-pinned crate results in a recoverable error
"""

import re
import os

from drivers.alr import run_alr, alr_pin
from drivers.asserts import assert_match
from glob import glob


# Add a dependency and force it missing by pinning it to non-existing version
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
run_alr('with', 'libhello^1')     # This causes libhello=1.1
alr_pin('libhello', version='1')  # Downgrade to 1.0

# Check that updating without specific crate does not err
run_alr('update')

# See that updating the pinned crate errs
p = run_alr('update', 'libhello', complain_on_error=False)
assert p.status != 0, "should have erred"

# Check that force updating the pinned crate does not err
run_alr('update', 'libhello', force=True)

# Check that the solution is still the expected one
p = run_alr('with', '--solve')
assert_match('.*Dependencies \(solution\):\n'
             '   libhello=1.0.0 \(pinned\).*',
             p.out, flags=re.S)


print('SUCCESS')
