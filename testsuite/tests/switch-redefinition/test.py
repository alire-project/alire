"""
Check that a command redefining a global switch is detected.
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq

# Run internal dev command with specific option for this test, first check:
p = run_alr('dev', '--check-switch-redefinition-1',
            quiet=False, # For this check, alr expects the exact above line
            complain_on_error=False)
assert p.status != 0, "alr should have errored"
assert_eq('ERROR: Redefined switch: -h\n' \
          + 'ERROR: alr encountered an unexpected error,' \
          + ' re-run with -d for details.\n', p.out)

# Run internal dev command with specific option for this test, second check:
p = run_alr('dev', '--check-switch-redefinition-2',
            quiet=False, # For this check, alr expects the exact above line
            complain_on_error=False)
assert p.status != 0, "alr should have errored"
assert_eq('ERROR: Redefined switch: --help\n' \
          + 'ERROR: alr encountered an unexpected error,' \
          + ' re-run with -d for details.\n', p.out)

print('SUCCESS')
