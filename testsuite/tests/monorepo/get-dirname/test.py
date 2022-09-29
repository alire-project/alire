"""
Verify the output of `alr get --dirname` when the crate is in a monorepo
"""

import os
import shutil

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import dir_separator

assert_eq(f"crate_12345678{dir_separator()}nested\n",
          run_alr("get", "crate", "--dirname").out)

print('SUCCESS')
