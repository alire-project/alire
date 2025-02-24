"""
Verify that no extraneous output is printed during `alr printenv` even when
sync/update needed.
"""

import os
import shutil
from drivers.alr import alr_lockfile, alr_with, init_local_crate, run_alr, run_alr_interactive
from drivers.asserts import assert_eq, assert_match


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

# Also compare to ground truth (specific paths are not important, "export" variables are)
assert_match("""\
export ALIRE="True"
export GPR_PROJECT_PATH=".*"
export HELLO_ALIRE_PREFIX="/.*/printenv__always-quiet/.*/hello_1.0.1_filesystem.*"
export LIBHELLO_ALIRE_PREFIX="/.*/printenv__always-quiet/.*/libhello_1.0.0_filesystem.*"
export TEST_ENV="myenv"
export TEST_GPR_EXTERNAL="gpr_ext_B"
export TEST_UNDECLARED="used_by_another_crate"
export XXX_ALIRE_PREFIX="/.*/printenv__always-quiet/xxx"
""",             
             p1.out)

# Test that a non-interactive run also completes without trying to interact or
# with unexpected output

p3 = run_alr_interactive(["printenv", "--unix"], [], [])
assert_eq(p2.out, p3)


print("SUCCESS")
