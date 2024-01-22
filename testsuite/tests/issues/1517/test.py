"""
In certain circumstances, deploying a dependency when its folder already exists
(?) causes a failure.
"""

from glob import glob
import os
import shutil
from drivers.alr import run_alr, crate_dirname

run_alr("get", "hello")
os.chdir(glob("hello_*")[0])

# Remove contens of the libhello dependency folder. The bug only happens if the
# top-level folder is not removed, so we need to iterate over its contents.
for dirpath, dirnames, filenames in \
    os.walk(f"alire/cache/dependencies/{crate_dirname('libhello')}"):
    for filename in filenames:
        os.remove(os.path.join(dirpath, filename))
    for dirname in dirnames:
        shutil.rmtree(os.path.join(dirpath, dirname))

# This should not fail after the bugfix
run_alr("build")

print("SUCCESS")
