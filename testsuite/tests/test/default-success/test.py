"""
Run a passing test
"""

import os.path

from drivers.alr import run_alr
from drivers.asserts import assert_match

run_alr("init", "--lib", "xxx")
os.chdir("xxx")

with open("./tests/src/xxx_tests-example_test.adb", "w") as f:
   f.write("""procedure Xxx_Tests.Example_Test is
begin
   null;
end Xxx_Tests.Example_Test;
""")

p = run_alr("test")
assert_match(".*\[ PASS \] example_test.*", p.out)

print('SUCCESS')
