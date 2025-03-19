"""
Check that tests deeper inside "src" subfolders are found and tested correctly,
and the name printed keeps the prefix inside "src".
"""

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_substring
from drivers.helpers import mkcd

# Initialize a local crate with a test
init_local_crate("xxx", with_test=True)

# Create a second test inside the "tests/src/nested" folder
from os import makedirs

# Create the nested directory structure
makedirs("tests/src/nested", exist_ok=True)

# Create a test file in the nested directory
with open("tests/src/nested/xxx_tests-nested_test.adb", "w") as f:
    f.write("""
procedure Xxx_Tests.Nested_Test is
begin
   null;
end Xxx_Tests.Nested_Test;
""")

# Run `alr test` and check that two PASS lines exist with the proper test names
p = run_alr("test")
assert_substring("[ PASS ] example_test", p.out)
assert_substring("[ PASS ] nested/nested_test", p.out)

print("SUCCESS")
