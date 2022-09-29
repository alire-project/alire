"""
Verify that requesting a pin from a dir which is not the root still works, but
the user gets a warning about it.
"""

import os, re

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import dir_separator

# Create top crate
init_local_crate()

# Create nested crate
init_local_crate("child")

# Enter a subdir of the nested crate
os.mkdir("sub")
os.chdir("sub")

s = re.escape(dir_separator())

# Add the pin for the parent crate from the subdir of the child
p = run_alr("with", "xxx", f"--use=..{s}..", quiet=False)

# Check the warning
assert_match(f".*Adding pin to .*pin__from-subdir{s}xxx in crate child rooted"
             f" at .*{s}pin__from-subdir{s}xxx{s}child",
             p.out)

print('SUCCESS')
