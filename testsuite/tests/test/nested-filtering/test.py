"""
Filter test runs by path in the builtin test runner
"""

import os.path

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match

def make_test(parent:str, name:str):
   cap = name[0].upper() + name[1:]
   os.makedirs(f"./tests/src/{parent}", exist_ok=True)
   with open(f"./tests/src/{parent}/xxx_tests-{name}.adb", "w") as f:
      f.write(f"""procedure Xxx_Tests.{cap} is
begin
   null;
end Xxx_Tests.{cap};
""")

init_local_crate("xxx", with_test=True)
os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

for parent, test in zip(["pos", "pos", "neg", "neg", "neg"],
                        ["yes1", "yes2", "yes3", "no1", "no2"]):
   make_test(parent, test)

# Checks on full path
assert run_alr("test", "pos").out.count("PASS") == 2
assert run_alr("test", "neg").out.count("PASS") == 3
assert run_alr("test", "s/y").out.count("PASS") == 2
assert run_alr("test", "g/y").out.count("PASS") == 1
assert run_alr("test", "g/n").out.count("PASS") == 2
assert run_alr("test", "/").out.count("PASS") == 5
assert run_alr("test", "po", "no").out.count("PASS") == 4

# Check on the test name only
run_alr("test").out.count("PASS") == 5
run_alr("test", "yes", "no").out.count("PASS") == 5
run_alr("test", "yes").out.count("PASS") == 3
run_alr("test", "no").out.count("PASS") == 2
run_alr("test", "anything").out.count("PASS") == 0

print('SUCCESS')
