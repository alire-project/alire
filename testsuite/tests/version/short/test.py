"""
Validate the output of alr --version
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

# Extract version from alr version
version = re.search("alr version:\s*?([^\s$]+)",
                    run_alr("version").out,
                    flags=re.M)[1]

# Match against the output we want
assert_eq(run_alr("--version").out, f"alr {version}\n")

print('SUCCESS')
