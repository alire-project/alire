"""
Check `alr test --toolchain`, that reports the tools each test runner would
build its tests with, without building.
"""

import json
import os

from drivers.alr import alr_manifest, alr_with, init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_match


def report(*args):
    """
    Return the structured report as a dict (runner, tool) -> row. Rows give
    the crate of the release in use, so compilers are identified by their
    "gnat" prefix. External runners have no tool, so their tool is None.
    """
    rows = json.loads(
        run_alr("--format=json", "test", "--toolchain", *args).out)
    result = {}
    for row in rows:
        crate = row["crate"]
        tool = (None if crate == ""
                else "gnat" if crate.startswith("gnat")
                else crate)
        key = (row["runner"], tool)
        assert key not in result, f"Duplicated {key} in {rows}"
        result[key] = row
    return result


run_alr("toolchain", "--select", "gnat_native=8888", "gprbuild=8888")

# A crate with two built-in runners, one of them requiring a cross compiler,
# and an external runner

init_local_crate() # This leaves us inside the crate, so next are test crates
init_local_crate("tests_1", enter=False)
init_local_crate("tests_2", enter=False)

os.chdir("tests_1")
alr_with("gnat_cross_1")
os.chdir("..")

with open(alr_manifest(), "a") as f:
    f.write("""
[[test]]
runner = 'alire'
directory = 'tests_1'
id = 'unit'

[[test]]
runner = 'alire'
directory = 'tests_2'

[[test]]
command = ['true']
directory = 'tests_2'
id = 'ext'
""")

r = report()
assert_eq({("unit", "gnat"), ("unit", "gprbuild"),
           ("tests_2", "gnat"), ("tests_2", "gprbuild"),
           ("ext", None)},
          set(r))
assert_eq({"runner": "unit", "crate": "gnat_cross_1", "version": "9999.0.0",
           "dependencies": "gnat_cross_1*", "source": "dependencies"},
          r[("unit", "gnat")])
assert_eq({"runner": "tests_2", "crate": "gnat_native", "version": "8888.0.0",
           "dependencies": "", "source": "global"},
          r[("tests_2", "gnat")])
for runner in ["unit", "tests_2"]:
    assert_eq({"runner": runner, "crate": "gprbuild", "version": "8888.0.0",
               "dependencies": "", "source": "global"},
              r[(runner, "gprbuild")])
assert_eq({"runner": "ext", "crate": "", "version": "",
           "dependencies": "", "source": "unknown"},
          r[("ext", None)])

# Plain output is the same table

assert_match(r"RUNNER\s+CRATE\s+VERSION\s+DEPENDENCIES\s+SOURCE\n"
             r"unit\s+gnat_cross_1\s+9999\.0\.0\s+gnat_cross_1\*\s+"
             r"dependencies\n",
             run_alr("test", "--toolchain").out)

# Selection of a single runner

assert_eq({("unit", "gnat"), ("unit", "gprbuild")},
          set(report("--id=unit")))

# A test crate without lockfile is solved first, as `alr test` does, both with
# and without dependencies.

for crate in ["tests_1", "tests_2"]:
    os.remove(os.path.join(crate, "alire", "alire.lock"))
r = report()
assert_eq("dependencies", r[("unit", "gnat")]["source"])
assert_eq("global", r[("tests_2", "gnat")]["source"])
for crate in ["tests_1", "tests_2"]:
    assert os.path.exists(os.path.join(crate, "alire", "alire.lock")), \
        f"Test crate {crate} should have been solved"

# Switches of a test run are rejected

p = run_alr("test", "--toolchain", "--list", complain_on_error=False)
assert p.status != 0, "Should have failed"
assert_match(".*--toolchain can only be combined with --id and --here.*",
             p.out)

# Verify nothing has been built

for folder in ["obj", "bin"]:
    for crate in [".", "tests_1", "tests_2"]:
        assert not os.path.exists(os.path.join(crate, folder)), \
            f"Unexpected build in {crate}"

# Without test runners, legacy test actions cannot be reported

os.chdir("..")
init_local_crate("no_runners")
p = run_alr("test", "--toolchain", complain_on_error=False)
assert p.status != 0, "Should have failed"
assert_match(".*No test runner defined; legacy test actions cannot be "
             "reported.*", p.out)

print('SUCCESS')
