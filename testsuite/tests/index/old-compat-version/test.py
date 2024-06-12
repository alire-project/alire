"""
Test that an old compatible index can be loaded, but a warning is given
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re
import os

# Enable the warning we are trying to test
run_alr("settings", "--global", "--set", "warning.old_index", "true")

# Run a command that loads the index. This produces a warning only, because the
# index version is old but valid.
p = run_alr("search", "--crates",  # Causes loading of the index
            quiet=False)

assert_match(".*Index 'community' version .* is older"
             " than the newest supported by alr.*",
             p.out)


print('SUCCESS')
