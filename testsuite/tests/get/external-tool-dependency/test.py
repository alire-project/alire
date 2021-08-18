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

# The directrory for the external dependencies 'make' contains a version number
# that can be different depending on the platform or version of the
# distribution. We search through the content to find that directory and use it
# in the expected output.
make_dep_dir = "__MAKE_DEPENDENCY_DIR_NOT_FOUND_"
for elt in dir_content:
    if re.match('^.*/alire/cache/dependencies/make_.*_external$', elt):
        make_dep_dir = elt

# Check folder contents
compare(dir_content,
        ['main_1.0.0_filesystem/alire',
         'main_1.0.0_filesystem/alire.toml',
         'main_1.0.0_filesystem/alire/alire.lock',
         'main_1.0.0_filesystem/alire/cache',
         'main_1.0.0_filesystem/alire/cache/dependencies',
         make_dep_dir,
         make_dep_dir + "/alire",
         'main_1.0.0_filesystem/config',
         'main_1.0.0_filesystem/config/main_config.ads',
         'main_1.0.0_filesystem/config/main_config.gpr',
         'main_1.0.0_filesystem/config/main_config.h',
         'main_1.0.0_filesystem/noop.gpr',
         'main_1.0.0_filesystem/src',
         'main_1.0.0_filesystem/src/noop.adb'
         ])

print('SUCCESS')
