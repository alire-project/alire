"""
Verify that `alr clean` does not rehash shared dependencies from the
environment it exports before generating the build configuration.
"""

import os

from drivers import builds
from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_file_exists


init_local_crate()
alr_with("envdep")

# Make gprclean require an environment variable exported by the dependency.
with open("xxx.gpr", "r+") as project:
    contents = project.read()
    project.seek(0)
    project.write(contents.replace(
        "project Xxx is",
        'project Xxx is\n\n   Envdep_Share := external ("ENVDEP_SHARE");'))
    project.truncate()

# Deploy the shared dependency without running its pre-build action.
run_alr("build", "--stop-after=generation")

build_dir = builds.find_dir("envdep")
assert_file_exists(builds.complete_copy_flag(build_dir))
assert_file_exists(os.path.join(build_dir, "pre_build_ran"), wanted=False)
hash_input = builds.hash_input("envdep")
base_folder = "envdep_1.0.0_filesystem"
expected_include = os.path.join(base_folder, "include")
expected_share = os.path.join(base_folder, "share")
assert f"environment:ENVDEP_INCLUDE={expected_include}" in hash_input
assert f"environment:ENVDEP_SHARE={expected_share}" in hash_input
assert build_dir.replace("/", os.sep) not in hash_input

# A fresh `alr clean` process must retain the same normalized build hash. In
# particular, its own exported ENVDEP_* values must not become hash inputs.
run_alr("clean")

assert builds.find_dir("envdep") == build_dir
assert_file_exists(os.path.join(build_dir, "pre_build_ran"), wanted=False)

print("SUCCESS")
