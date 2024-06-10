"""
Test listing of actions with `alr action`
"""

import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match
from glob import glob

expected_output = """main=1.0.0:
   Post_Fetch run: echo POST-FETCH MAIN (from ${CRATE_ROOT}/.)
"""

# First test, list action in a root crate
run_alr("get", "main")
os.chdir(glob("main*")[0])
p = run_alr("action")
assert_eq(expected_output, p.out)


# Second test, check that action in dependency is not listed by default
os.chdir("..")
init_local_crate()
alr_with("main")
p = run_alr("action")
assert_eq("No actions.\n", p.out)

# Third test, check that action in dependency is listed with --recursive, -r
p1 = run_alr("action", "--recursive")
p2 = run_alr("action", "-r")
assert_eq(expected_output, p1.out)
assert_eq(expected_output, p2.out)

print('SUCCESS')
