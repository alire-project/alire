"""
Test the output when a external crate is in the dependencies
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

import re
import platform


# Retrieve a crate with an external dependency
run_alr('get', 'libhello=0.9-test_unav_native', '--force')
os.chdir('libhello_0.9.0_filesystem')

# Run it not quietly to ensure that at normal level
# the output is not broken by some log message
p = run_alr('setenv', quiet=False)
assert_eq(0, p.status)

expected_gpr_path=""
if platform.system() == 'Windows':
    expected_gpr_path = '.*\\\\libhello_0.9.0_filesystem'
else:
    expected_gpr_path = '.*/libhello_0.9.0_filesystem'

# Check the setenv output
assert_match('warn: Generating incomplete environment'  # Note: this warning is
             ' because of missing dependencies\n'       # via stderr so it's OK
             'export GPR_PROJECT_PATH="' + expected_gpr_path + '"\n'
             'export ALIRE="True"\n',
             p.out)

print('SUCCESS')
