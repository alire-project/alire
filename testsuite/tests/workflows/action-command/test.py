"""
Test pre-build/post-build/post-fetch executions
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import compare, contents
from pathlib import Path

import os

def check_expected(expected):
    if not expected in contents('.'):
        assert False, "%s expected in %s\n Got: %s" % \
               (expected, dir, str(contents(dir)))

def check_not_expected(expected):
    if expected in contents('.'):
        assert False, "%s is unexpected in %s\n Got: %s" % \
               (expected, dir, str(contents(dir)))


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
p = run_alr('build', 'hello_world', complain_on_error=False)
assert_match(".*compilation of empty.adb failed.*", p.out)

# Post build shouldn't be here because of build failure
check_not_expected('./test_post_fetch')
check_expected('./test_pre_build')
check_not_expected('./test_post_build')

os.remove('./test_pre_build')
os.remove('src/empty.adb')

# Build without error
run_alr('build', 'hello_world', complain_on_error=False)

# pre/post-build expected for successful build
check_not_expected('./test_post_fetch')
check_expected('./test_pre_build')
check_expected('./test_post_build')

print('SUCCESS')
