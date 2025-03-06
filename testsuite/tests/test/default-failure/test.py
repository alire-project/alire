"""
Run a failing test and check it is detected correctly
"""

import os.path

from drivers.alr import run_alr
from drivers.asserts import assert_match

run_alr("init", "--lib", "xxx")
os.chdir("xxx")

with open("./tests/src/xxx_tests-example_test.adb", "w") as f:
   f.write("""procedure Xxx_Tests.Example_Test is
begin
   raise Program_Error;
end Xxx_Tests.Example_Test;
""")

p = run_alr("test", complain_on_error=False)
assert_match(".*\[ FAIL \] example_test.*", p.out)

print('SUCCESS')
