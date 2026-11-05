"""
`alr build --toolchain` confirms the compiler of each project file by having
gprbuild compile an empty unit as one of its sources, so it reports the
compiler that gprbuild actually runs, without leaving anything on disk
"""

import json
import os
import shutil
import subprocess

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import add_to_project, exe_name

PROJECT = "xxx.gpr"


def gnat_row():
    """
    The row of the compiler in the structured report
    """
    rows = json.loads(run_alr("--format=json", "build", "--toolchain").out)
    rows = [row for row in rows if row["crate"].startswith("gnat")]
    assert_eq(1, len(rows))
    return rows[0]


def assert_no_leftovers():
    """
    The probe and its compilation output are gone
    """
    for path, dirs, files in os.walk("."):
        for name in dirs + files:
            assert not name.startswith("alire_toolchain_probe"), \
                f"Probe left at {os.path.join(path, name)}"


def restore_project():
    """
    Undo the changes to the project file
    """
    with open(PROJECT, "w") as file:
        file.write(original)


init_local_crate()
with open(PROJECT) as file:
    original = file.read()

# The compiler decided by Alire, detected in the environment, is the one
# gprbuild runs: the report is the same, with the path of the compiler.

row = gnat_row()
assert_eq("gnat_external", row["crate"])
assert_eq("environment", row["source"])
assert_eq(PROJECT, row["project"])
assert os.path.isfile(row["path"]), f"Not a compiler: {row['path']}"
assert_no_leftovers()
assert not os.path.exists("obj"), "Unexpected object dir"

# A compiler given by the project file is not the one decided by Alire: the
# report gives it instead, as found by gprbuild, and warns about it.

compiler = os.path.abspath(os.path.join("fakebin", exe_name("gcc")))
os.mkdir("fakebin")
shutil.copy(row["path"], compiler)
version = subprocess.run([compiler, "-dumpfullversion", "-dumpversion"],
                         capture_output=True, text=True).stdout.strip()

add_to_project(PROJECT, f'for Driver ("Ada") use "{compiler}";',
               package="Compiler")

row = gnat_row()
assert_eq("gnat_external", row["crate"])
assert_eq("gprbuild", row["source"])
assert_eq(compiler, row["path"])
assert row["version"].startswith(version), \
    f"Version {row['version']} is not {version}"
assert_match(f".*gprbuild compiles {PROJECT} with gnat_external.*, not with "
             "gnat_external.* as decided by Alire.*",
             run_alr("build", "--toolchain", quiet=False).out)
assert_no_leftovers()

# A compiler that cannot be found leaves Alire's decision, and a warning with
# gprbuild's reasons, which depend on its version.

restore_project()
add_to_project(PROJECT, 'for Target use "alire-nonexistent-target";')

row = gnat_row()
assert_eq("environment", row["source"])
assert_eq("", row["path"])
assert_match(".*Could not confirm with gprbuild the compiler for "
             f"{PROJECT}: .*",
             run_alr("build", "--toolchain", quiet=False).out)
assert_no_leftovers()

# Without probing, nothing is asked to gprbuild

restore_project()
rows = json.loads(run_alr("--format=json", "build", "--toolchain",
                          "--no-probe").out)
assert all(row["path"] == "" for row in rows), f"Unexpected paths: {rows}"

print("SUCCESS")
