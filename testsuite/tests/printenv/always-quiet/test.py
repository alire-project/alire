"""
Verify that no extraneous output is printed during `alr printenv` even when
sync/update needed.
"""

import os
import shutil
from drivers.alr import alr_lockfile, alr_with, init_local_crate, run_alr, run_alr_interactive
from drivers.asserts import assert_eq


def check_output(output : str):
    # Split in lines and verify that every line is an export    
    for line in output.splitlines():
        assert line.startswith("export ")


#  We create a new crate with some dependencies and delete the `alire` forder.
#  This forces a sync that normally would print stuff that is unwanted during
#  `alr printenv`.

init_local_crate()
alr_with("hello")

shutil.rmtree("alire")

# This one will perform a silent sync
p1 = run_alr("printenv", "--unix", quiet=False)  # This one would fail <2.1

# Verify the sync happened
assert os.path.isfile(alr_lockfile())

# A second, quiet printenv should always work properly
p2 = run_alr("printenv", "--unix", quiet=True)

# Output should match
assert_eq(p1.out, p2.out)

# Also check that every line is an export and not something else.
# We do not check the specific contents as they vary between OSes and build modes.
check_output(p1.out)

# Test that a non-interactive run also completes without trying to interact or
# with unexpected output

p3 = run_alr_interactive(["printenv", "--unix"], [], [])
assert_eq(p2.out, p3)


print("SUCCESS")
