"""
`alr test --toolchain` confirms with gprbuild the compiler of the test crates,
unless they share their object dir with another project, like their parent
crate, which gprbuild rejects for the probe.
"""

import json
import os
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import add_to_project, exe_name, replace_in_file

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
    Neither the probe nor the object dir of the test crate remain. gprbuild may
    create the object dirs of dependencies, like the parent crate, as a build
    would.
    """
    for path, dirs, files in os.walk("."):
        for name in dirs + files:
            assert not name.startswith("alire_toolchain_probe"), \
                f"Probe left at {os.path.join(path, name)}"
    obj = os.path.join("tests", "obj")
    assert not os.path.exists(obj), f"Unexpected object dir {obj}"


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

# Sharing the object dir of the parent crate makes the probe a source of both
# projects, which gprbuild rejects: Alire's decision is reported with a warning.

replace_in_file(os.path.join("tests", PROJECT),
                'Object_Dir use "obj/"', 'Object_Dir use "../obj/"')

row = gnat_row()
assert_eq("environment", row["source"])
assert_eq("", row["path"])
assert_match(".*Could not confirm with gprbuild the compiler for "
             f"{PROJECT}: .*",
             run_alr("test", "--toolchain", quiet=False).out)
assert_no_leftovers()

print("SUCCESS")
