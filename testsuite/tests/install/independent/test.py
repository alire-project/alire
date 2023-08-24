"""
Test installation of two independent crates with `alr install` to verify that
our deployment system doesn't introduce any conflicts.
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq, assert_match, assert_installed
from subprocess import run

import os


PREFIX=os.path.join(os.getcwd(), "install")
PREFIX_ARG=f"--prefix={PREFIX}"

# Install both crates one after the other, shouldn't fail
run_alr("install", PREFIX_ARG, "crate1")
run_alr("install", PREFIX_ARG, "crate2")

# Check contents of the prefix
assert_installed(PREFIX, ["crate1=1.0.0", "crate2=1.0.0"])

print('SUCCESS')