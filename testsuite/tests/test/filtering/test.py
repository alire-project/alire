"""
Filter test runs by name in the builtin test runner
"""

import os.path

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match

def make_test(name: str):
   cap = name[0].upper() + name[1:]
   with open(f"./tests/src/xxx_tests-{name}.adb", "w") as f:
      f.write(f"""procedure Xxx_Tests.{cap} is
begin
   null;
end Xxx_Tests.{cap};
""")

init_local_crate("xxx", with_test=True)
os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

for test in ["yes1", "yes2", "yes3", "no1", "no2"]:
   make_test(test)

p = run_alr("test")
assert p.out.count("PASS") == 5

p = run_alr("test", "yes", "no")
assert p.out.count("PASS") == 5

p = run_alr("test", "yes")
assert p.out.count("PASS") == 3

p = run_alr("test", "no")
assert p.out.count("PASS") == 2

p = run_alr("test", "anything")
assert p.out.count("PASS") == 0

p = run_alr("test", "xxx")
assert p.out.count("PASS") == 0 # No match on common prefix

print('SUCCESS')
