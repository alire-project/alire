"""
Run a failing test and check its output is suppressed when using -q
"""

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_substring, assert_match

init_local_crate("xxx", with_test=True)

with open("./tests/src/xxx_tests-assertions_enabled.adb", "w") as f:
    f.write("""procedure Xxx_Tests.Assertions_Enabled is
begin
   raise Program_Error;
end Xxx_Tests.Assertions_Enabled;
""")

# check that when -q is set to false, the failing test output is displayed
p = run_alr("test", complain_on_error=False, quiet=False)
assert_match(".*\[ FAIL \] *\d+[smh]\d+ assertions_enabled.*", p.out)
assert_substring("raised PROGRAM_ERROR : xxx_tests-assertions_enabled.adb", p.out)

# check that when -q is passed, the failing test output is suppressed
p = run_alr("test", complain_on_error=False, quiet=True)
assert_match(".*\[ FAIL \] *\d+[smh]\d+ assertions_enabled.*", p.out)
assert "raised PROGRAM_ERROR : xxx_tests-assertions_enabled.adb" not in p.out

print("SUCCESS")
