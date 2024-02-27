"""
Test the hinting with custom text in external definitions
"""

from glob import glob

from drivers.alr import distro_is_known, run_alr
from drivers.asserts import assert_eq, assert_match

import re
import platform

# 1st test: directly attempting to retrieve an external (this is doable for
# system externals in supported platforms -- never in this test). Depending on
# whether the distro has a supported package manager we get two outcomes:

p = run_alr('get', 'crate', quiet=False, complain_on_error=False)

if distro_is_known():
    assert_match(".*Hint: This is a custom hint.*", p.out, flags=re.S)
else:
    assert_eq('ERROR: No source release indexed for the requested crate, and '
              'cannot use system packages in unknown distribution\n',
              p.out)

# 2nd test: hint is displayed when the hint belongs to a dependency, on get

p = run_alr('get', 'crate_master', force=True, quiet=False)

assert_match
(".*"  # Skip previous user interaction and warning about incomplete solution
 "Warning: The following native dependencies are unavailable within Alire:\n"
 "Warning:    crate\*\n"
 "Warning:       Hint: This is a custom hint\n"
 "Warning: They should be made available in the environment by the user.\n",
 p.out)

# 3rd test: hint is displayed when showing the crate info

p = run_alr('show', 'crate_master', '--solve', '--system', quiet=False)

assert_match(".*"   # Skip previous crate info
             "Dependencies \(missing\):\n"
             "   crate\* \(direct,hinted\)\n"
             "      Hint: This is a custom hint\n"
             "Dependencies \(graph\):\n"
             ".*",  # Skip solution graph
             p.out, flags=re.S)

print('SUCCESS')
