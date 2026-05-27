"""
Run a some tests and check the expected structured output
"""

import os
import json
import yaml
import toml

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

    assert sorted(list(data["summary"].keys())) == ["failures", "total"]
    assert data["summary"]["total"] == 2
    assert data["summary"]["failures"] == 1


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
