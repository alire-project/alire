"""
Check that tests deeper inside "src" subfolders are found and tested correctly,
and the name printed keeps the prefix inside "src".
"""

from os import makedirs

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_substring
from drivers.helpers import mkcd

# Initialize a local crate with a test
init_local_crate("xxx", with_test=True)

# Create a second test inside the "tests/src/nested" folder

makedirs("tests/src/nested", exist_ok=True)

with open("tests/src/nested/xxx_tests-nested_test.adb", "w") as f:
    f.write("""
procedure Xxx_Tests.Nested_Test is
begin
   null;
end Xxx_Tests.Nested_Test;
""")

# Run `alr test` and check that two PASS lines exist with the proper test names

p = run_alr("test")
assert_substring("[ PASS ] assertions_enabled", p.out)
assert_substring("[ PASS ] nested/nested_test", p.out)

# Create a third failing test to check its failure and output are reported

with open("tests/src/nested/xxx_tests-failing_test.adb", "w") as f:
    f.write("""
procedure Xxx_Tests.Failing_Test is
begin
   raise Program_Error;
end Xxx_Tests.Failing_Test;
""")

p = run_alr("test", complain_on_error=False)
# We check piecemeal as exact output varies between OSes (macOS in particular)
assert_substring("[ FAIL ] nested/failing_test", p.out)
assert_substring("raised PROGRAM_ERROR : xxx_tests-failing_test.adb", p.out)

print("SUCCESS")
