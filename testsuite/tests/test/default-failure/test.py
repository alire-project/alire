"""
Run a failing test and check it is detected correctly
"""

import os.path

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match

init_local_crate("xxx", with_test=True)

# Check with explicit exception

with open("./tests/src/xxx_tests-assertions_enabled.adb", "w") as f:
   f.write("""procedure Xxx_Tests.Assertions_Enabled is
begin
   raise Program_Error;
end Xxx_Tests.Assertions_Enabled;
""")

p = run_alr("test", complain_on_error=False)
assert_match(".*\[ FAIL \] assertions_enabled.*", p.out)

# Check with plain assertion (verify that assertions are evaluated)

with open("./tests/src/xxx_tests-assertions_enabled.adb", "w") as f:
   f.write("""procedure Xxx_Tests.Assertions_Enabled is
begin
   pragma Assert (False);
end Xxx_Tests.Assertions_Enabled;
""")

p = run_alr("test", complain_on_error=False)
assert_match(".*\[ FAIL \] assertions_enabled.*", p.out)

print('SUCCESS')
