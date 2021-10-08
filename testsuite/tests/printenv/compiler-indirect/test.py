"""
This is an attempt to replicate the bug fixed in #875. The root cause was that
a crate preceding "gnat" alphabetically was used to provide the environment for
GNAT, due to a mistype in the function that returns the concrete crate that
fills in for a virtual crate.

Hence, we setup a root crate, that depends on "gnat" and "another_crate" (which
comes first alphabetically), and verify the environment contains the compiler.
"""

import os
import re
import subprocess

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import dir_separator

# Install a binary compiler for later use
run_alr("toolchain", "--select", "gnat_native")

init_local_crate("another_crate")  # This enters the crate
alr_with("gnat")  # Make it depend on gnat (in the original bug, the dependency
# was also indirect, although it would have been a problem with a direct
# dependency as well).
os.chdir("..")  # Back to root folder

init_local_crate()
alr_with(path="../another_crate", manual=False)  # Autodetect the linked crate

p = run_alr("printenv")

s = re.escape(dir_separator())

# Ensure the compiler-set flag is in there:
assert_match(".*export TEST_PATH=.*"
             f"printenv__compiler-indirect{s}alr-config{s}cache{s}"
             f"dependencies{s}gnat_native_8888\.0\.0_99fa3a55{s}bin",
             p.out)

print('SUCCESS')
