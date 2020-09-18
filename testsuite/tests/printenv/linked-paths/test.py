"""
Check that both base path, and a extra project path for a softlinked crate is
properly added to the environment
"""

import os
import re
import platform

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import dir_separator, path_separator

# Initialize test crate
run_alr("init", "--bin", "xxx")
os.chdir("xxx")

# Link a folder which also contains crate metadata
run_alr("with", "--use=../my_index/crates/crate_1234")

expected_gpr_path = []
expected_gpr_path += [['.*', 'my_index', 'crates', 'crate_1234']]
expected_gpr_path += [['.*', 'my_index', 'crates', 'crate_1234', 'nested']]
expected_gpr_path += [['.*', 'xxx']]

for i, path in enumerate(expected_gpr_path):
    if platform.system() == 'Windows':
        expected_gpr_path[i] = "\\\\".join(path)
    else:
        expected_gpr_path[i] = "/".join(path)

expected_gpr_path = os.pathsep.join(expected_gpr_path)

# Check paths are proper (base and one extra nested)
p = run_alr("printenv")
assert_match(('export GPR_PROJECT_PATH="' + expected_gpr_path + '"\n' +
              'export ALIRE="True"\n').replace('/', re.escape(dir_separator())),
             p.out)


print('SUCCESS')
