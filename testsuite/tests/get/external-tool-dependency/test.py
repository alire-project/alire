"""
Test that external tool dependencies work as expected.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents

# Should silently retrieve everything
p = run_alr('get', 'main')
assert_eq('', p.out)

# Check folder contents
compare(contents('main_1.0.0_filesystem/'),
        ['main_1.0.0_filesystem/alire',
         'main_1.0.0_filesystem/alire/main.lock',
         'main_1.0.0_filesystem/alire/main.toml',
         'main_1.0.0_filesystem/noop.gpr',
         'main_1.0.0_filesystem/src',
         'main_1.0.0_filesystem/src/noop.adb'
         ])

print('SUCCESS')
