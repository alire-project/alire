"""
Test pre-build/post-build/post-fetch executions in a crate that is a dependency
"""

import os
from glob import glob
from pathlib import Path
from shutil import rmtree

from drivers import builds
from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_match
from drivers.helpers import contents, neutral_path


def check_expected(expected, path):
    if not (expected in contents(path)):
        assert False, "%s expected in %s\n Got: %s" % \
               (expected, path, str(contents(path)))


def check_not_expected(expected, path):
    if expected in contents(path):
        assert False, "%s is unexpected in %s\n Got: %s" % \
               (expected, path, str(contents(path)))


def check(file, to_be_or_not_to_be, path):
    if to_be_or_not_to_be:
        check_expected(file, path)
    else:
        check_not_expected(file, path)


def do_checks(path_to_dependency):
    flag_post_fetch = path_to_dependency + "/test_post_fetch"
    flag_pre_build = path_to_dependency + "/test_pre_build"
    flag_post_build = path_to_dependency + "/test_post_build"

    # Immediately after adding the dependency, this is the situation:
    check(flag_post_fetch, False, path_to_dependency)
    check(flag_pre_build, False, path_to_dependency)
    check(flag_post_build, False, path_to_dependency)

    # Build with error, so only pre-build runs but not post-build
    Path(f"{path_to_dependency}/src/empty.adb").touch()
    p = run_alr('build', complain_on_error=False)
    assert_match(".*compilation of empty.adb failed.*", p.out)

    # Post build shouldn't be here because of build failure; post-fetch should
    # however now exist because a build has been attempted and post-fetch
    # succeeded (even if the build failed at a later stage)
    check(flag_post_fetch, True, path_to_dependency)
    check(flag_pre_build, True, path_to_dependency)
    check(flag_post_build, False, path_to_dependency)

    # Remove post-fetch to check it doesn't come back unexpectedly
    os.remove(flag_post_fetch)

    # Post build shouldn't be here because of build failure
    check(flag_post_fetch, False, path_to_dependency)
    check(flag_pre_build, True, path_to_dependency)
    check(flag_post_build, False, path_to_dependency)

    os.remove(flag_pre_build)
    os.remove(f"{path_to_dependency}/src/empty.adb")

    # Build without error
    run_alr('build')

    # pre/post-build expected for successful build
    check(flag_post_fetch, False, path_to_dependency)
    check(flag_pre_build, True, path_to_dependency)
    check(flag_post_build, True, path_to_dependency)
    return


# Initialize a crate and add as dependency the crate that contains the triggers
init_local_crate("root", binary=False)  # Lib so all contents are compiled
alr_with("hello_world")
run_alr("update")
# Test all triggers
if builds.are_shared():
    base = neutral_path(builds.find_dir("hello_world"))
else:
    base = neutral_path(glob("./alire/cache/dependencies/hello*")[0])
do_checks(base)

# Repeat tests, for a crate that has been added as a linked dependency
os.chdir("..")
rmtree("root")
run_alr("get", "hello_world")
hello = glob("hello*")[0].replace('\\', '/')
init_local_crate("root", binary=False)
os.rename(f"../{hello}", f"./{hello}")
alr_with(path=f"{hello}", manual=False)
do_checks(f"./{hello}")

print('SUCCESS')
