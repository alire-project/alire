"""
Paths in environment values should use native path separators in the index, and
be converted to the native separator for usage.
"""

import os
import re

from drivers.alr import crate_dirname, run_alr
from drivers.asserts import assert_match
from drivers.helpers import content_of

# Check that the output of `alr show` shows the proper slashes

# Show uses the path with portable separators, as the conversion is done just
# for printenv output.
assert_match(f".*/crate_test_bin",
             run_alr("show", "crate").out)

# Similarly, if we retrieve the crate, the output to the manifest must be in
# portable format (always forward slashes).
run_alr("get", "crate")
assert_match(f".*/crate_test_bin",
             content_of(f"{crate_dirname('crate')}/alire.toml"))

# When obtained via printenv, the path must be native
os.chdir(crate_dirname('crate'))
assert_match(".*PATH=[^\n]*" + re.escape(f"{os.sep}crate_test_bin"),
             run_alr("printenv").out)

print('SUCCESS')
