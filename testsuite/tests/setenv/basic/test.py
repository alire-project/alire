"""
Test the environment set for a basic crate
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

# Run it not quietly to ensure that at normal level
# the output is not broken by some log message
p = run_alr('setenv', quiet=False)
assert_eq(0, p.status)

if platform.system() == 'Windows':
    assert_match('export TEST_GPR_EXTERNAL="gpr_ext_B"\n'
                 'export GPR_PROJECT_PATH="[A-Z]:\\\\.*\\\\alire\\\\cache\\\\dependencies\\\\libhello_1\.0\.0_filesystem"\n'
                 'export ALIRE="True"\n',
                 p.out, flags=re.S)
else:
    assert_match('export TEST_GPR_EXTERNAL="gpr_ext_B"\n'
                 'export GPR_PROJECT_PATH="/.*/alire/cache/dependencies/libhello_1\.0\.0_filesystem"\n'
                 'export ALIRE="True"\n',
                 p.out, flags=re.S)

print('SUCCESS')
