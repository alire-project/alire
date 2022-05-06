"""
Verify the proper solving of dependencies with `alr show --solve <crate>`
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

p = run_alr("show", "hello", "--solve")

# Should not be missing dependencies
assert_match(".*" +
             re.escape("""
Dependencies (solution):
   libhello=1.0.0
Dependencies (graph):
   hello=1.0.1 --> libhello=1.0.0 (^1.0)
""") +
             ".*",
             p.out)

print('SUCCESS')
