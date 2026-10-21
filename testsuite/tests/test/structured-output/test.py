"""
Run a some tests and check the expected structured output
"""

import os
import json
import yaml
import toml

from drivers.alr import init_local_crate, run_alr
from drivers.testing import write_test

init_local_crate("xxx", with_test=True)

os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

write_test("failing_test", "raise Program_Error;")
write_test("passing_test", "null;")


def structure_tests(data):
    assert sorted(list(data.keys())) == ["summary", "tests"]

    tests = list(data["tests"])
    tests.sort(key = lambda it: it["name"])
    assert list(map(lambda it: it["name"], tests)) == ["failing_test", "passing_test"]
    print(tests[0])
    assert sorted(list(tests[0].keys())) == [
        "duration",
        "name",
        "output",
        "reason",
        "status",
    ]
    assert sorted(list(tests[1].keys())) == [
        "duration",
        "name",
        "status",
    ]
    assert tests[0]["status"] == "fail"
    assert tests[1]["status"] == "pass"

    assert sorted(list(data["summary"].keys())) == [
        "failures",
        "force_ignored",
        "skipped",
        "total",
    ]
    assert data["summary"]["total"] == 2
    assert data["summary"]["failures"] == 1
    assert data["summary"]["skipped"] == 0
    assert data["summary"]["force_ignored"] == 0


p = run_alr("--format=json", "test", complain_on_error=False)
data = json.loads(p.out)
structure_tests(data)

p = run_alr("--format=yaml", "test", complain_on_error=False)
data = yaml.safe_load(p.out)
structure_tests(data)

p = run_alr("--format=toml", "test", complain_on_error=False)
data = toml.loads(p.out)
structure_tests(data)

print("SUCCESS")
