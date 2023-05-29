"""
Test that external tool dependencies work as expected.
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents

from subprocess import check_output

# Should silently retrieve everything
p = run_alr('get', 'main')
assert_eq('', p.out)

dir_content = contents('main_1.0.0_filesystem/')

# Check folder contents. Shared/external/system dependencies should not
# generate a folder in the cache directory.
compare(dir_content,
        ['main_1.0.0_filesystem/alire',
         'main_1.0.0_filesystem/alire.toml',
         'main_1.0.0_filesystem/alire/alire.lock',
         'main_1.0.0_filesystem/alire/config.toml',
         'main_1.0.0_filesystem/config',
         'main_1.0.0_filesystem/config/main_config.ads',
         'main_1.0.0_filesystem/config/main_config.gpr',
         'main_1.0.0_filesystem/config/main_config.h',
         'main_1.0.0_filesystem/noop.gpr',
         'main_1.0.0_filesystem/src',
         'main_1.0.0_filesystem/src/noop.adb'
         ])

print('SUCCESS')
