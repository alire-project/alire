"""
Verify the output of `alr get --dirname` when the crate is in a monorepo
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import dir_separator

import re

# The folder is named after the repository, a hash of its URL, and the commit
assert_match(f"crate_[0-9a-f]{{8}}_12345678{re.escape(dir_separator())}nested\n",
             run_alr("get", "crate", "--dirname").out)

print('SUCCESS')
