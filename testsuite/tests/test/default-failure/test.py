"""
Run a failing test and check it is detected correctly
"""

import os.path

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match
from drivers.testing import write_test

init_local_crate("xxx", with_test=True)

# Check with explicit exception

write_test("assertions_enabled", "raise Program_Error;")

p = run_alr("test", complain_on_error=False)
assert_match(".*\[ FAIL \] *\d+[smh]\d+ assertions_enabled.*", p.out)

# Check with plain assertion (verify that assertions are evaluated)

write_test("assertions_enabled", "pragma Assert (False);")

p = run_alr("test", complain_on_error=False)
assert_match(".*\[ FAIL \] *\d+[smh]\d+ assertions_enabled.*", p.out)

print('SUCCESS')
