"""
Test proper syncing of softlinks, even bad ones

This test is Unix-only, as Windows' tar cannot recreate the broken links:

crate-0.1.0
└── crate
    ├── bin -> subdir/bin
    ├── broken -> missing
    ├── lib
    │   ├── mock.so -> mock.so.0.0
    │   ├── mock.so.0 -> mock.so.0.0
    │   ├── mock.so.0.0
    │   ├── zzz.so -> mock.so
    │   └── zzz.so.0 -> mock.so
    ├── loop
    │   ├── x -> z
    │   ├── y -> x
    │   └── z -> y
    ├── order
    │   ├── ab -> b
    │   ├── af -> d/f
    │   ├── b
    │   ├── cb -> b
    │   ├── d
    │   │   └── f
    │   └── zf -> d/f
    ├── self -> self
    ├── subdir
    │   ├── bin
    │   │   ├── loop -> ../../subdir
    │   │   └── x
    │   ├── parent -> ..
    │   └── self -> ../subdir
    ├── that -> this
    └── this -> that

"""

import os
import shutil
import drivers.builds as builds
from drivers.alr import alr_with, init_local_crate, run_alr


init_local_crate()

# Make the crate depend on our troublesome crate and ensure syncing
alr_with("crate")
builds.sync()

# Ensure that a copy has been made to the cache
if builds.are_shared():
    assert os.path.exists(builds.find_dir("crate"))

# Cleanup
os.chdir("..")
shutil.rmtree("xxx")

print('SUCCESS')
