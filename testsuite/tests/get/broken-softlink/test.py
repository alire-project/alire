"""
Regression test for a source archive containing a dangling symlink.

Windows' tar (msys2/git's) cannot recreate a symlink whose target does not
exist: it tries to copy the target instead of creating an actual link, and
fails with "Cannot create symlink ...: No such file or directory". `alr`
used to let this abort the whole deployment, and even worse, on Windows it
masked the real error behind an unrelated one from a blind retry without
`--force-local` (see the `Untar` procedure in
alire-origins-deployers-source_archive.adb).

`alr` must now tolerate this and just warn, so `get` succeeds on every
platform. Whether the broken link itself ends up on disk is platform
dependent (it does on Unix, it does not on Windows), so we do not assert on
it, only on the rest of the crate contents.
"""


import os
import shutil

from drivers.alr import crate_dirname, run_alr


run_alr("get", "crate")

cratedir = crate_dirname("crate")
assert os.path.isdir(cratedir), f"Missing expected crate dir: {cratedir}"
assert os.path.isfile(os.path.join(cratedir, "crate", "x")), \
    f"Missing expected file 'crate/x' in {cratedir}"

# Clean up ourselves: on platforms where the dangling symlink could be
# created (i.e., not Windows), the testsuite driver's own cleanup walks the
# tree with os.chmod, which follows symlinks and fails on a dangling one.
shutil.rmtree(cratedir)

print('SUCCESS')
