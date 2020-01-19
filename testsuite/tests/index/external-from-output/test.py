"""
Test detection of tools in path via external
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

import re

# Hint that an external exists
p = run_alr('show', 'make',
            complain_on_error=False, quiet=False)
assert_eq('Not found: make with Newest version\n'
          'There are external definitions for the crate. '
          'Use --external to show them.\n',
          p.out)

# External definition
p = run_alr('show', 'make', '--external')
assert_eq('Kind       Description    Details           \n'
          'Executable make --version .*Make ([\d\.]+).*\n',
          p.out)

# External detection
p = run_alr('show', 'make', '--external-detect')
assert_match('make=.*: Utility for directing compilation\n'
             'Notes: Detected at .*make(\.exe)?\n'
             'Origin: external path .*make(\.exe)?\n',
             p.out, flags=re.S)

print('SUCCESS')
