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

expected_gpr_path = []
expected_gpr_path += [['.*', 'hello_1.0.1_filesystem']]
expected_gpr_path += [['.*', 'alire', 'cache', 'dependencies', 'libhello_1\.0\.0_filesystem']]

for i, path in enumerate(expected_gpr_path):
    if platform.system() == 'Windows':
        expected_gpr_path[i] = "\\\\".join(path)
    else:
        expected_gpr_path[i] = "/".join(path)

expected_gpr_path = os.pathsep.join(expected_gpr_path)

assert_match('export TEST_GPR_EXTERNAL="gpr_ext_B"\n'
             'export GPR_PROJECT_PATH="' + expected_gpr_path + '"\n'
             'export ALIRE="True"\n',
             p.out, flags=re.S)


print('SUCCESS')
