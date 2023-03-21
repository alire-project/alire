"""
Test installation of an indexed crate
"""

from drivers.alr import run_alr
from drivers.asserts import assert_installed, assert_match

import os

prefix = os.path.join(os.getcwd(), "prefix")
prefix_arg = f"--prefix={prefix}"

run_alr("install", prefix_arg, "noop=1.0.0-multi")
assert_installed(prefix, ["noop=1.0.0-multi"])

# Check that reinstallation detects already installed
p = run_alr("install", prefix_arg, "noop=1.0.0-multi", quiet=False)
assert_match(".*Skipping already installed noop=1.0.0-multi.*", p.out)

# Check that trying to install a different version doesn't fly
p = run_alr("install", prefix_arg, "noop=1.0.0-nondef", complain_on_error=False)
assert_match(".*Release noop=1.0.0-nondef has another version already installed.*", p.out)

# Check that we can force and the new version is installed, and the old one is gone
p = run_alr("install", prefix_arg, "noop=1.0.0-nondef", force=True, quiet=False)
assert_installed(prefix, ["noop=1.0.0-nondef"])


print('SUCCESS')
