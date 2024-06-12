"""
Test --chdir switch.
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_match

test_dir = os.getcwd()

# Make a binary crate in test that it runs without cd'ing.
run_alr("init", "--bin", "xxx")
run_alr("--chdir=xxx", "run")

# Test that changing to a non-existent directory fails. Technically,
# /dev/null/cantexist can exist, but it's more than reasonable to assume a
# system with this directory has bigger issues.
bad_chdir = run_alr("-C", "/dev/null/cantexist", "run", complain_on_error=False)
assert_match(".*directory \"/dev/null/cantexist\" does not exist.*", bad_chdir.out)

print('SUCCESS')
