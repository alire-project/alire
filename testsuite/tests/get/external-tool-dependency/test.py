"""
Test that external tool dependencies work as expected.
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents

from subprocess import check_output

# Identify make version we are going to see reported
regex = re.compile("GNU Make ([0-9.]+).*")
out = check_output(['make', '--version'], universal_newlines=True)
version = regex.match(out).group(1)
while version.count('.') < 2:  # ensure a complete version, e.g., 4.1.0
    version = version + '.0'

# Should silently retrieve everything
p = run_alr('get', 'main')
assert_eq('', p.out)

# Check folder contents
compare(contents('main_1.0.0_filesystem/'),
        ['main_1.0.0_filesystem/alire',
         'main_1.0.0_filesystem/alire/cache',
         'main_1.0.0_filesystem/alire/cache/dependencies',
         'main_1.0.0_filesystem/alire/cache/dependencies/make_' + version + '_external',
         'main_1.0.0_filesystem/alire/main.lock',
         'main_1.0.0_filesystem/alire/main.toml',
         'main_1.0.0_filesystem/noop.gpr',
         'main_1.0.0_filesystem/src',
         'main_1.0.0_filesystem/src/noop.adb'
         ])

print('SUCCESS')
