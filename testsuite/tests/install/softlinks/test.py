"""
Test that binary files containing softlinks can be installed properly. The test
crate contains all kinds of pernicious links (broken, recursive, etc.):

crate/
├── bin -> subdir/bin
├── broken -> missing
├── lib
│   ├── mock.so -> mock.so.0.0
│   ├── mock.so.0 -> mock.so.0.0
│   ├── mock.so.0.0
│   ├── zzz.so -> mock.so
│   └── zzz.so.0 -> mock.so
├── order
│   ├── ab -> b
│   ├── af -> d/f
│   ├── b
│   ├── cb -> b
│   ├── d
│   │   └── f
│   └── zf -> d/f
└── subdir
    ├── bin
    │   ├── loop -> ../../subdir
    │   └── x
    ├── parent -> ..
    └── self -> ../subdir

"""

from drivers.alr import run_alr
from drivers.helpers import on_windows


# Does not apply to Windows as it does not support softlinks
if on_windows():
    print('SKIP: on Windows, unapplicable')
    sys.exit(0)

# This command should succeed normally
run_alr("install", "--prefix=install", "crate")

# Contents should be identical. For that, we first untar the crate and then
# directly compare with the destination.

run_alr("get", "crate") # This merely untars and moves the whole dir in one step,
                        # so contents are not modified.
cratedir = crate_dirname("crate")
os.chdir(cratedir)
shutil.rmtree("alire")  # Created by get
os.remove("alire.toml") # Created by get
items = contents(".")   # Contents of the original crate
os.chdir("..")
os.chdir("install")     # Contents of the install prefix

for item in items:
    orig = f"../{cratedir}/{item}"
    whatis = kind(orig)
    if os.path.islink(orig) and os.path.isdir(orig):
        continue # We aren't yet able to copy those
    if not os.path.exists (orig):
        continue # Broken links, we don't copy them at all
    assert os.path.exists(item), \
        f"Missing expected entry {item}: {whatis}') in " + \
        f"contents (dst):\n{ls('.')}" + \
        f"contents (src):\n{ls('../' + cratedir)}"
    assert kind(item) == kind(orig), \
        f"Unexpected kind for {item}: {kind(item)} != {kind(orig)}"

# Cleanup
os.chdir("..")
shutil.rmtree(cratedir)

print('SUCCESS')
