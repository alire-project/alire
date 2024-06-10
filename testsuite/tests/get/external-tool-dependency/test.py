"""
Test that external tool dependencies work as expected.
"""

import re

from drivers import builds
from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents

from subprocess import check_output

# Should silently retrieve everything
p = run_alr('get', 'main')
assert_eq('', p.out)

dir_content = contents('main_1.0.0_filesystem/')

# The directory for the external dependencies 'make' contains a version number
# that can be different depending on the platform or version of the
# distribution. We search through the content to find that directory and use it
# in the expected output.
make_dep_dir = "__MAKE_DEPENDENCY_DIR_NOT_FOUND_"
for elt in dir_content:
    if re.match('^.*/alire/cache/dependencies/make_.*_external$', elt):
        make_dep_dir = elt

# These only appear if dependencies are sandboxed
extra = ['main_1.0.0_filesystem/alire/cache',
         'main_1.0.0_filesystem/alire/cache/dependencies',
         make_dep_dir,
         make_dep_dir + "/alire",
         make_dep_dir + "/alire/flags",
         make_dep_dir + "/alire/flags/complete_copy"]

# Check folder contents
compare(dir_content,
        ['main_1.0.0_filesystem/alire',
         'main_1.0.0_filesystem/alire.toml',
         'main_1.0.0_filesystem/alire/alire.lock'] +
         (extra if not builds.are_shared() else []) +
        ['main_1.0.0_filesystem/alire/flags',
         'main_1.0.0_filesystem/alire/flags/complete_copy',
         'main_1.0.0_filesystem/noop.gpr',
         'main_1.0.0_filesystem/src',
         'main_1.0.0_filesystem/src/noop.adb'
         ])

if builds.are_shared():
    # External tools that have no sources don't have a shared build dir:
    try:
        assert builds.find_dir("make")  # This should raise
        raise Exception("Should not have found make build dir")
    except:
        pass

print('SUCCESS')
