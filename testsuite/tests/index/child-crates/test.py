"""
Test the loading of child crates.
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_eq

import os

# LISTING
p = run_alr('list')
assert_eq("parent        A parent package         \n"
          "parent.child  A child of parent package\n",
          p.out)

# SEARCHING
p = run_alr('search', '--list')
assert_eq("NAME          STATUS  VERSION  DESCRIPTION                NOTES\n"
          "parent                1.0.0    A parent package                \n"
          "parent.child          1.0.0    A child of parent package       \n",
          p.out)

# RETRIEVING
# Child crates share their folder with their parent, so that's what's verified.

# First, enter a clean directory
os.mkdir('xxx')
os.chdir('xxx')

# Do the retrieval and tests
run_alr('get', 'parent')        # First retrieval
run_alr('get', 'parent.child')  # Second retrieval reuses the folder silently

# Ensure it was retrieved
assert os.path.isdir('parent_1.0.0_filesystem'), "Expected folder not found"
# And only it was retrieved
assert len(os.listdir('.')) == 1, "Too many entries in folder"

print('SUCCESS')
