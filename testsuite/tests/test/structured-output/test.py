"""
Run a some tests and check the expected structured output
"""

import os
import json
import yaml

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

    assert sorted(list(data["tests"].keys())) == ["failing_test", "passing_test"]
    assert sorted(list(data["tests"]["failing_test"].keys())) == [
        "output",
        "reason",
        "status",
    ]
    assert data["tests"]["failing_test"]["status"] == "fail"
    assert data["tests"]["passing_test"]["status"] == "pass"

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
try:
    import tomllib
    data = tomllib.loads(p.out)
    structure_tests(data)
except ModuleNotFoundError:
    pass

print("SUCCESS")
