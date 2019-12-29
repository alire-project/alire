"""
Verify that creating a Outcome_Failure also dumps the stack trace to log output
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

p = run_alr('index', '--name', 'xx', '--add', '.',
            complain_on_error=False, debug=True, quiet=True)
# Failed call because name is too short. That causes a Outcome_Failure to be
# returned.

# Since stack traces wildly differ across platforms, a minimal check is done:
assert_match(
    '.*Generating Outcome_Failure with call stack:.*',
    p.out, flags=re.S)

print('SUCCESS')
