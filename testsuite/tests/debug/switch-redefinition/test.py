"""
Check that a command redefining a global switch is detected.
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Run internal dev command with specific option for this test, first check:
p = run_alr('dev', '--check-switch-redefinition-1',
            complain_on_error=False)
assert p.status != 0, "alr should have errored"
assert_match('ERROR: Duplicate switch definition detected\n.*', p.out)

# Run internal dev command with specific option for this test, second check:
p = run_alr('dev', '--check-switch-redefinition-2',
            complain_on_error=False)
assert p.status != 0, "alr should have errored"
assert_match('ERROR: Duplicate switch definition detected\n.*', p.out)

print('SUCCESS')
