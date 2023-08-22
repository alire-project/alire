"""
Test that binary files containing softlinks can be installed properly. The test
crate contains all kinds of pernicious links (broken, recursive, etc.):

crate
├── bin -> subdir/bin
├── broken -> missing
└── subdir
    ├── bin
    │   ├── loop -> ../../subdir
    │   └── x
    ├── parent -> ..
    └── self -> ../subdir
"""

import sys

from drivers.alr import run_alr
from drivers.helpers import on_windows


# Does not apply to Windows as it does not support softlinks
if on_windows():
    print('SKIP: on Windows, unapplicable')
    sys.exit(0)

# This command should succeed normally
run_alr("install", "--prefix=install", "crate")


print('SUCCESS')
