"""
Test that an old compatible index can be loaded, but a warning is given
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re
import os

# Run a command that loads the index. This errs with a too old index version
p = run_alr("search", "--crates",  # Causes loading of the index
            complain_on_error=False)

assert_match(".*" + re.escape("index version (0.0.0) is too old.") +
             " The minimum compatible version is .*",
             p.out)


print('SUCCESS')
