"""
Check that an env var can be set twice to the same value
"""

import os
import re

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match

# Create a local workspace and enter it
init_local_crate()

# checkparent depends on checkenv that sets the same environment variable
run_alr("with", "checkparent")

print('SUCCESS')
