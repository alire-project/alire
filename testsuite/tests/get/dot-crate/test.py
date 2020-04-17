"""
Test retrieval and build for a crate with hierarchical name (with dots)
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents

import os
import re

# Retrieve
p = run_alr('get', 'xxx.yyy.zzz')
assert_eq('', p.out)

# Check folder contents
os.chdir('xxx.yyy.zzz_1.0.0_filesystem')
compare(contents('.'),
        ['./alire',
         './alire/xxx-yyy-zzz.toml',
         './src',
         './src/xxx_yyy_zzz.adb',
         './xxx_yyy_zzz.gpr'
         ])

# Verify the build succeeds
run_alr('build')


print('SUCCESS')
