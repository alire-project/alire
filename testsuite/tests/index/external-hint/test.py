"""
Test the hinting with custom text in external definitions
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re
import platform

# 1st test: directly attempting to retrieve an external (this is doable for
# system externals in supported platforms -- never in this test)

p = run_alr('get', 'crate', quiet=False, complain_on_error=False)

assert_match("Hint: This is a custom hint.*",
             p.out, flags=re.S)

# 2nd test: hint is displayed when the hint belongs to a dependency, on get

p = run_alr('get', 'crate_master', quiet=False)

assert_match
("Warning: The following native dependencies are unavailable within Alire:\n"
 "Warning:    crate\*\n"
 "Warning:       Hint: This is a custom hint\n"
 "Warning: They should be made available in the environment by the user.\n",
 p.out)

# 3rd test: hint is displayed when showing the crate info

p = run_alr('show', 'crate_master', '--solve', '--system', quiet=False)

assert_match(".*Dependencies \(external\):\n"
             "   crate\*\n"
             "      Hint: This is a custom hint\n.*",
             p.out, flags=re.S)

print('SUCCESS')
