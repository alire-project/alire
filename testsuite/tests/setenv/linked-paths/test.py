"""
Check that both base path, and a extra project path for a softlinked crate is
properly added to the environment
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import path_separator

# Initialize test crate
run_alr("init", "--bin", "xxx")
os.chdir("xxx")

# Link a folder with also contains crate definitions
run_alr("with", "--use=../my_index/crates/crate_1234")

# Check paths are proper (base and one extra nested)
s = path_separator()
p = run_alr("setenv")
assert_match('export GPR_PROJECT_PATH=".*/my_index/crates/crate_1234'
             ':.*/my_index/crates/crate_1234/nested/"\n'
             'export ALIRE="True"\n'.replace('/', re.escape(path_separator())),
             p.out)


print('SUCCESS')
