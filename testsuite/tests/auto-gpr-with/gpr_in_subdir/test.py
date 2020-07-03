"""
Test the GPR_PROJECT_PATH for a project file in sub dirs
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

import re
import platform
from drivers.helpers import check_line_in

# Get the "hello" project and enter its directory
run_alr('init', '--bin', 'myhello')
os.chdir(glob('myhello*')[0])

# Get dependencies that should also add a with statement in myhello.gpr
run_alr('with', 'libhello')
run_alr('with', 'gpr_in_subdir')

check_line_in('myhello.gpr', 'with "libhello.gpr";')
check_line_in('myhello.gpr', 'with "gpr_in_subdir.gpr";')
print('SUCCESS')
