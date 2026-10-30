"""
`alr test --toolchain` confirms with gprbuild the compiler of the test crates.
The default test crate shares the object dir of its parent crate, so its
configuration spec is checked instead of compiling a probe.
"""

import json
import os
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import add_to_project, exe_name

PROJECT = "xxx_tests.gpr"


def gnat_row():
    """
    The row of the compiler of the only runner in the structured report
    """
    rows = json.loads(run_alr("--format=json", "test", "--toolchain").out)
    rows = [row for row in rows if row["crate"].startswith("gnat")]
    assert_eq(1, len(rows))
    return rows[0]


def assert_no_leftovers():
    """
    Neither the probe nor any object dir remain
    """
    for path, dirs, files in os.walk("."):
        for name in dirs + files:
            assert not name.startswith("alire_toolchain_probe"), \
                f"Probe left at {os.path.join(path, name)}"
    assert not os.path.exists("obj"), "Unexpected object dir"


init_local_crate(with_test=True)

# The compiler decided by Alire is the one gprbuild runs

row = gnat_row()
assert_eq("tests", row["runner"])
assert_eq("gnat_external", row["crate"])
assert_eq("environment", row["source"])
assert_eq(PROJECT, row["project"])
assert os.path.isfile(row["path"]), f"Not a compiler: {row['path']}"
assert_no_leftovers()

# A compiler given by the project file of the test crate is reported instead

compiler = os.path.abspath(os.path.join("fakebin", exe_name("gcc")))
os.mkdir("fakebin")
shutil.copy(row["path"], compiler)
add_to_project(os.path.join("tests", PROJECT),
               f'for Driver ("Ada") use "{compiler}";', package="Compiler")

row = gnat_row()
assert_eq("gprbuild", row["source"])
assert_eq(compiler, row["path"])
assert_match(f".*gprbuild compiles {PROJECT} with gnat_external.*",
             run_alr("test", "--toolchain", quiet=False).out)
assert_no_leftovers()

print("SUCCESS")
