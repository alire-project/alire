"""
Fix for issue https://github.com/alire-project/alire/issues/1801
Ensure that the proper error is given when trying to pin a crate twice
"""

import os
import re

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match, assert_substring
from drivers.helpers import dir_separator, init_git_repo
from glob import glob

# We can trigger the bug condition locally by using a local git repo as the
# remote pin. We are going to pin twice a nested crate (the nesting is
# irrelevant).

TARGET = 'nested'

# Our main crate
init_local_crate()

# Create the nested crate and initialize a repo there
init_local_crate(TARGET, enter=False)
init_git_repo(TARGET)

# Pin first time
run_alr("with", TARGET, f"--use=git+file:{TARGET}")

# Verify pin is correct
assert_substring(f"git+file:{TARGET}", run_alr("pin").out)

# Verify proper error on 2nd attempt
assert_substring(f"{TARGET} is already pinned with pin url=git+file:{TARGET}",
                  run_alr("with", TARGET, f"--use=git+file:{TARGET}",
                          complain_on_error=False).out)

print('SUCCESS')
