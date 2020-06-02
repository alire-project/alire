"""
Ensure that no unexpected output appears in the console. This happened in the
past, where the finalization of something was causing an extra empty line.
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

# Check a few commands for unexpected output

# Commands that require session
assert_match(".*Cannot continue with invalid session:"  # skip logging prefix
             " Could not detect a session folder"
             " at current or parent locations\n",
             run_alr('with', quiet=False, complain_on_error=False).out)

# Commands within a trivial session
assert_eq("",
          run_alr('init', '--bin', 'xxx').out)
os.chdir('xxx')

assert_eq("",
          run_alr('update').out)

assert_eq("Nothing to update.\n",
          run_alr('update', quiet=False).out)

assert_eq("Dependencies (direct):\n"
          "   (empty)\n",
          run_alr('with', quiet=False).out)

print('SUCCESS')
