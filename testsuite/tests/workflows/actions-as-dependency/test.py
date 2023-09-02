"""
Test pre-build/post-build/post-fetch executions in a crate that is a dependency
"""

from drivers.alr import run_alr, init_local_crate, add_action, alr_with
from drivers.asserts import assert_match
from drivers.helpers import compare, contents, on_windows
from glob import glob
from pathlib import Path
from shutil import rmtree

import os


def check_expected(expected):
    if not (expected in contents('.')):
        assert False, "%s expected in %s\n Got: %s" % \
               (expected, '.', str(contents('.')))


def check_not_expected(expected):
    if expected in contents('.'):
        assert False, "%s is unexpected in %s\n Got: %s" % \
               (expected, '.', str(contents('.')))


def check(file, to_be_or_not_to_be):
    if to_be_or_not_to_be:
        check_expected(file)
    else:
        check_not_expected(file)


def do_checks(path_to_dependency):
    flag_post_fetch = path_to_dependency + "/test_post_fetch"
    flag_pre_build = path_to_dependency + "/test_pre_build"
    flag_post_build = path_to_dependency + "/test_post_build"

    # Immediately after adding the dependency, this is the situation:
    check(flag_post_fetch, False)
    check(flag_pre_build, False)
    check(flag_post_build, False)

    # Build with error, so only pre-build runs but not post-build
    Path(f"{path_to_dependency}/src/empty.adb").touch()
    p = run_alr('build', complain_on_error=False)
    assert_match(".*compilation of empty.adb failed.*", p.out)

    # Post build shouldn't be here because of build failure
    check(flag_post_fetch, True)
    check(flag_pre_build, True)
    check(flag_post_build, False)

    # Remove post-fetch to check it doesn't come back unexpectedly
    os.remove(flag_post_fetch)

    # Post build shouldn't be here because of build failure
    check(flag_post_fetch, False)
    check(flag_pre_build, True)
    check(flag_post_build, False)

    os.remove(flag_pre_build)
    os.remove(f"{path_to_dependency}/src/empty.adb")

    # Build without error
    run_alr('build')

    # pre/post-build expected for successful build
    check(flag_post_fetch, False)
    check(flag_pre_build, True)
    check(flag_post_build, True)
    return

    # updating dependencies causes the post-fetch action to run:
    run_alr('update')
    check(flag_post_fetch, True)
    check(flag_pre_build, True)
    check(flag_post_build, True)


# Initialize a crate and add as dependency the crate that contains the triggers
init_local_crate("root", binary=False)  # Lib so all contents are compiled
alr_with("hello_world")
# Test all triggers
do_checks(glob("./alire/cache/dependencies/hello*")[0].replace('\\', '/'))

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
