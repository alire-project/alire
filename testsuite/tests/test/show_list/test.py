"""
Run alr test with the --list flag to get test details before running them
"""
import os
import json

from drivers.alr import init_local_crate, run_alr

init_local_crate("xxx", with_test=True)

os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

with open("./tests/src/xxx_tests-failing_test.adb", "w+") as f:
    f.write("""procedure Xxx_Tests.Failing_Test is
begin
   raise Program_Error;
end Xxx_Tests.Failing_Test;
""")

with open("./tests/src/xxx_tests-passing_test.adb", "w+") as f:
    f.write("""procedure Xxx_Tests.Passing_Test is
begin
   null;
end Xxx_Tests.Passing_Test;
""")

p = run_alr("test", "--list", quiet = False)
print(p.out)
assert "failing_test" in p.out
assert "passing_test" in p.out

p = run_alr("--format=json", "test", "--list")
data = json.loads(p.out)
assert list(data.keys()) == ["tests"]

tests = list(data["tests"])
tests.sort(key = lambda it: str(it["name"]))
assert len(tests) == 2

assert tests[0]["name"] == "failing_test"
assert tests[0]["path"].endswith("tests/src/xxx_tests-failing_test.adb")

assert tests[1]["name"] == "passing_test"
assert tests[1]["path"].endswith("tests/src/xxx_tests-passing_test.adb")

print("SUCCESS")
