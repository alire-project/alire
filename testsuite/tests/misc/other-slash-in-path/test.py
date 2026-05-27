"""
Check that slashes from other OSes in paths are handled correctly.
Fix for issue #1984
"""

import os
from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_substring
from drivers.helpers import on_windows

BAD_SLASH = '/' if on_windows() else '\\'
BAD_DIR = f"test{BAD_SLASH}crate"
START_DIR = os.getcwd()

# Initialize a crate in place within a directory with the contrary slash
# to the one used by the current OS.

os.mkdir(BAD_DIR)
os.chdir(BAD_DIR)
run_alr("init", "testcrate", "--bin", "--in-place")

# Check we are were we expect to be
assert_eq(os.getcwd(), os.path.join(START_DIR, BAD_DIR))

# Check that it can be shown
p = run_alr("show")

# Verify the crate name is in the output
assert_substring("testcrate=0.1.0-dev", p.out)

print("SUCCESS")
