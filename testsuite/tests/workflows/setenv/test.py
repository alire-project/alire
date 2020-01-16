"""
Test a basic get-build-run workflow.
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

import re
import platform


# Get the "hello" project and enter its directory
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])

p = run_alr('setenv')
assert_eq(0, p.status)

if platform.system() == 'Windows':
    assert_match('export GPR_PROJECT_PATH="[A-Z]:\\\\.*\\\\alire\\\\cache\\\\dependencies\\\\libhello_1\.0\.0_filesystem"\n'
                 'export TEST_GPR_EXTERNAL="gpr_ext_B"\n'
                 'export ALIRE="True"\n',
                 p.out, flags=re.S)
else:
    assert_match('export GPR_PROJECT_PATH="/.*/alire/cache/dependencies/libhello_1\.0\.0_filesystem"\n'
                 'export TEST_GPR_EXTERNAL="gpr_ext_B"\n'
                 'export ALIRE="True"\n',
                 p.out, flags=re.S)

print('SUCCESS')
