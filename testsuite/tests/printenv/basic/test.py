"""
Test the environment set for a basic crate
"""

from glob import glob
import os

from drivers import builds
from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

import re
import platform


# Get the "hello" project and enter its directory
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])

# Run it not quietly to ensure that at normal level
# the output is not broken by some log message
p = run_alr('printenv', quiet=False)
assert_eq(0, p.status)

def make_path(list):
    if platform.system() == 'Windows':
        return "\\\\".join(list)
    else:
        return "/".join(list)

#  Force dependency syncing for the shared case
run_alr("update")

expected_hello_path = make_path(['.*', 'hello_1.0.1_filesystem'])
expected_libhello_path = \
make_path(['.*', os.path.basename(builds.find_dir('libhello'))]) \
if builds.are_shared() else \
make_path(['.*', 'alire', 'cache', 'dependencies', 'libhello_1\.0\.0_filesystem'])

#  Depending on the dependency location, both orders may occur
expected_gpr_path = \
    "(" \
    f"{os.pathsep.join([expected_hello_path, expected_libhello_path])}" \
    "|" \
    f"{os.pathsep.join([expected_libhello_path, expected_hello_path])}" \
    ")"

assert_match('export ALIRE="True"\n'
             '.*'
             'export GPR_PROJECT_PATH="' + expected_gpr_path + '"\n'
             '.*'
             'export HELLO_ALIRE_PREFIX="' + expected_hello_path + '"\n'
             '.*'
             'export LIBHELLO_ALIRE_PREFIX="' + expected_libhello_path + '"\n'
             '.*'
             'export TEST_GPR_EXTERNAL="gpr_ext_B"\n'
             '.*',
             p.out, flags=re.S)

# Verify that project paths are prepended before anything that the user already
# has configured in the environment, so for example GNAT libraries do not get
# mixed (compiler in PATH is ours but libraries are from the user).
os.environ["GPR_PROJECT_PATH"] = "canary"
assert_match(
    '.*'
    f'export GPR_PROJECT_PATH="{expected_gpr_path}{os.pathsep}canary"\n',
    run_alr('printenv', quiet=False).out)

print('SUCCESS')
