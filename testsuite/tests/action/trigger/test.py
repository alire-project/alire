"""
Test triggering of actions with `alr action`
"""

import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match
from glob import glob

expected_output = "POST-FETCH MAIN\n"

# Test trigger action in a root crate
run_alr("get", "main")
os.chdir(glob("main*")[0])
p = run_alr("action", "post-fetch")
assert_eq(expected_output, p.out)

# Test trigger of undefined action
p = run_alr("action", "post-build")
assert_eq("No actions to run.\n", p.out)

# Test trigger of non-existent action
p = run_alr("action", "made-up", complain_on_error=False)
assert_eq("ERROR: Invalid action: made-up\n", p.out)

# Test that action in dependency is not triggered by default
os.chdir("..")
init_local_crate()
alr_with("main")
p = run_alr("action", "post-fetch")
assert_eq("No actions to run.\n", p.out)

# Third test, check that action in dependency is listed with --recursive, -r
p = run_alr("action", "post-fetch", "--recursive")
assert_eq(expected_output, p.out)

print('SUCCESS')
