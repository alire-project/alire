"""
Test detection of tools in path via external
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

import re

# Hint that an external exists
p = run_alr('show', 'make', quiet=False)
assert_eq('Not found: make*\n'
          'There are external definitions for the crate. '
          'Use --external to show them.\n',
          p.out)

# External definition
p = run_alr('show', 'make', '--external')
assert_eq('Kind       Description    Details            Available\n'
          'Executable make --version .*Make ([\\d\\.]+).* True\n',
          p.out)

# External detection
p = run_alr('show', 'make', '--external-detect')
assert_match('make=.*: Utility for directing compilation\n'
             'Notes: Detected at .*make(\.exe)?\n'
             'Origin: external path .*make(\.exe)?\n',
             p.out, flags=re.S)

# Verify that an invalid command does not crash Alire. We don't want different
# behaviors in different platforms to unsuspectedly break users on an index
# update.

p = run_alr("show", "bad_switch", quiet=False)
assert p.status == 0, "unexpected exit with error"
assert_match(".*There are external definitions for the crate.",
             p.out)

# External definition check (crate is actually there)
p = run_alr('show', 'bad_switch', '--external')
assert_eq('Kind       Description                   '
          'Details            Available\n'
          'Executable make --bad-nonexistent-switch '
          '.*Make ([\\d\\.]+).* True\n',
          p.out)

# External detection fails (no release found, but without error)
p = run_alr('show', 'bad_switch', '--external-detect', quiet=False)
assert_match('.*Not found: bad_switch', p.out)

print('SUCCESS')
