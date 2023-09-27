"""
Test the with statement for a project file in sub dirs
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

# In shared mode, a "with" won't generate config files yet (which is desirable
# in case the withed crate has configuration variables without defaults). So we
# need to trigger the generation of the config files.
run_alr("update")

check_line_in('config/myhello_config.gpr', 'with "libhello.gpr";')

# When the crate declares a project file: `dir1/dir2/dir3/prj.gpr`, the with
# statement has to be `with "prj.gpr"`. Without the sub-dirs because
# GPR_PROJECT_PATH is already set with dirs that contain the project file.
check_line_in('config/myhello_config.gpr', 'with "gpr_in_subdir.gpr";')

print('SUCCESS')
