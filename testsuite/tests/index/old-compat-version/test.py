"""
Test that an old compatible index can be loaded, but a warning is given
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re
import os

# Run a command that loads the index. This produces a warning only, because the
# index version is old but valid.
p = run_alr("search", "--crates",  # Causes loading of the index
            quiet=False)

assert_match(".*" + re.escape("Index 'community' version (1.0.99) is older") +
             " than the newest supported by alr.*",
             p.out)


print('SUCCESS')
