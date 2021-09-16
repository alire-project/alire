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
run_alr('get', 'libhello=0.9-test-unav-native', force=True)
os.chdir('libhello_0.9.0_filesystem')

# Run it not quietly to ensure that at normal level
# the output is not broken by some log message
p = run_alr('printenv', quiet=False)
assert_eq(0, p.status)

expected_gpr_path = ""
if platform.system() == 'Windows':
    expected_gpr_path = '.*\\\\libhello_0.9.0_filesystem'
else:
    expected_gpr_path = '.*/libhello_0.9.0_filesystem'

# Check the printenv output
assert_match('warn: Generating possibly incomplete environment'
             ' because of missing dependencies\n'
             # Note: this warning is via stderr so it's OK
             '.*'
             'export ALIRE="True"\n'
             '.*'
             'export GPR_PROJECT_PATH="' + expected_gpr_path + '"\n'
             '.*',
             p.out)

print('SUCCESS')
