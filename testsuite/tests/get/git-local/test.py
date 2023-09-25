"""
Retrieve a release from a local git repository
"""

from glob import glob

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match
from drivers.helpers import compare, contents

# Get the release
p = run_alr('get', 'libfoo')

# Check expected contents (excepting .git contents)
compare(list(filter
             (lambda str: ".git" not in str,
              contents('libfoo_1.0.0_9ddda32b'))),
        ['libfoo_1.0.0_9ddda32b/a',
         'libfoo_1.0.0_9ddda32b/alire',
         'libfoo_1.0.0_9ddda32b/alire.toml',
         'libfoo_1.0.0_9ddda32b/alire/alire.lock',
         'libfoo_1.0.0_9ddda32b/alire/flags',
         'libfoo_1.0.0_9ddda32b/alire/flags/complete_copy',
         'libfoo_1.0.0_9ddda32b/b',
         'libfoo_1.0.0_9ddda32b/b/x',
         'libfoo_1.0.0_9ddda32b/b/y',
         'libfoo_1.0.0_9ddda32b/b/y/p',
         'libfoo_1.0.0_9ddda32b/b/y/q',
         'libfoo_1.0.0_9ddda32b/b/z',
         'libfoo_1.0.0_9ddda32b/c',
         ])

# Check as dependency
init_local_crate()
run_alr("with", "libfoo")  # should succeed

print('SUCCESS')
