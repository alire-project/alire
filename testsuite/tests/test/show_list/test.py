"""
Run alr test with the --list flag to get test details before running them
"""
import os

from drivers.alr import init_local_crate, run_alr
from drivers.testing import parse_json_result, write_test

init_local_crate("xxx", with_test=True)

os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

write_test("failing_test", "raise Program_Error;")
write_test("passing_test", "null;")

p = run_alr("test", "--list", quiet = False)
print(p.out)
assert "failing_test" in p.out
assert "passing_test" in p.out

p = run_alr("--format=json", "test", "--list")
data = parse_json_result(p)
assert list(data.keys()) == ["tests"]

tests = list(data["tests"])
tests.sort(key = lambda it: str(it["name"]))
assert len(tests) == 2

assert tests[0]["name"] == "failing_test"
assert tests[0]["path"].endswith("tests/src/xxx_tests-failing_test.adb")

assert tests[1]["name"] == "passing_test"
assert tests[1]["path"].endswith("tests/src/xxx_tests-passing_test.adb")

print("SUCCESS")
