"""
Run a passing test
"""

import os.path

from drivers.alr import run_alr
from drivers.asserts import assert_match

run_alr("init", "--lib", "xxx")
os.chdir("xxx")

with open("./tests/src/tests-example_test.adb", "w") as f:
   f.write("""procedure Tests.Example_Test is
begin
   null;
end Tests.Example_Test;
""")

p = run_alr("test")
assert_match(".*\[ PASS \] [^\n]*tests-example_test.*", p.out)

print('SUCCESS')
