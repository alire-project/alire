"""
Test init command produced artifacts and options
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents

# Get crate from tarball and check contents
run_alr('get', 'libhello=1.0.0-tarball')
compare(contents('libhello_1.0.0_tarball_99fa3a55'),
        ['libhello_1.0.0_tarball_99fa3a55/alire',
         'libhello_1.0.0_tarball_99fa3a55/alire.toml',
         'libhello_1.0.0_tarball_99fa3a55/alire/alire.lock',
         'libhello_1.0.0_tarball_99fa3a55/alire/flags',
         'libhello_1.0.0_tarball_99fa3a55/alire/flags/complete_copy',
         'libhello_1.0.0_tarball_99fa3a55/libhello.gpr',
         'libhello_1.0.0_tarball_99fa3a55/src',
         'libhello_1.0.0_tarball_99fa3a55/src/libhello.adb',
         'libhello_1.0.0_tarball_99fa3a55/src/libhello.ads',
         ])

print('SUCCESS')
