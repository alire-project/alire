"""
Test pre-build/post-build/post-fetch executions on a crate that is the root
"""

from drivers.alr import run_alr, init_local_crate, add_action, alr_with
from drivers.asserts import assert_match
from drivers.helpers import compare, contents, on_windows
from pathlib import Path

import os


def check_expected(expected):
    if not (expected in contents('.')):
        assert False, "%s expected in %s\n Got: %s" % \
               (expected, '.', str(contents('.')))


def check_not_expected(expected):
    if expected in contents('.'):
        assert False, "%s is unexpected in %s\n Got: %s" % \
               (expected, '.', str(contents('.')))


# Get and check post fetch action
run_alr('get', 'hello_world')
os.chdir("hello_world_0.1.0_filesystem/")
check_expected('./test_post_fetch')
check_not_expected('./test_pre_build')
check_not_expected('./test_post_build')

# Remove post-fetch to check it doesn't come back
os.remove('./test_post_fetch')

# Build with error
os.mkdir('src')
Path('src/empty.adb').touch()
p = run_alr('build', complain_on_error=False)
assert_match(".*compilation of empty.adb failed.*", p.out)

# Post build shouldn't be here because of build failure
check_not_expected('./test_post_fetch')
check_expected('./test_pre_build')
check_not_expected('./test_post_build')

os.remove('./test_pre_build')
os.remove('src/empty.adb')

# Build without error
run_alr('build')

# pre/post-build expected for successful build
check_not_expected('./test_post_fetch')
check_expected('./test_pre_build')
check_expected('./test_post_build')

# updating dependencies causes the post-fetch action on the root crate to run:
run_alr('update')
check_expected('./test_post_fetch')
check_expected('./test_pre_build')
check_expected('./test_post_build')

# Add a linked dependency. Since these are never "fetched", in order to
# complete their action cycle, post-fetch is also run on updates

init_local_crate("depended", binary=False, enter=True)
# Add a similar action
if on_windows():
    add_action("post-fetch", ["cmd", "/C", "copy NUL test_post_fetch_dep"])
else:
    add_action("post-fetch", ["touch", "test_post_fetch_dep"])


check_not_expected('./test_post_fetch_dep')
os.chdir("..")  # Back to parent crate
alr_with("depended", path="depended", update=False)
run_alr("update")
check_not_expected('./test_post_fetch_dep')
check_expected('./depended/test_post_fetch_dep')

print('SUCCESS')
