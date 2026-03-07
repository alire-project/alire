"""
Check basic use: manual install and listing of toolchains
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

# Install a precise version of gnat
run_alr("toolchain", "--select", "gnat_native=7777")

# Verify that it appears as default
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("7777.0.0") + ".*Default",
             p.out)

# Also that the external compiler is detected and always available
assert_match(".*gnat_external.*Available", p.out)

# Repeat install but without giving a version, and one should be autoidentified
# as the newest available version
p = run_alr("toolchain", "--select", "gnat_native", quiet=False)
assert_match(".*Requested crate resolved as gnat_native=8888.0.0.*",
             p.out)

# Verify that we can explicitly install a second version for the same target
run_alr("toolchain", "--select", "gnat_native=7777")
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("7777.0.0") + ".*Available",
             p.out)


print('SUCCESS')
