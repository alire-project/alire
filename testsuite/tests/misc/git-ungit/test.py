"""
Verify the proper "ungitting" of git origins
"""

import os
import shutil
from glob import glob

from drivers.alr import crate_dirname, run_alr
from drivers.asserts import assert_file_exists

# By default, git deployments are shallow and see their .git directory removed
# Check that and that enabling legacy behavior works

cwd = os.getcwd()

run_alr("get", "hello")
os.chdir(crate_dirname("hello"))

# Check root gotten crate
assert_file_exists(".git", wanted=False)

# Check dependency
os.chdir(os.path.join("alire", "cache", "dependencies"))
assert_file_exists(os.path.join(glob("libhello*")[0], ".git"), wanted=False)

print('SUCCESS')
