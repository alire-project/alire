"""
Check basic use: manual install, uninstall, and listing of toolchains
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

# Install a precise version of gnat
run_alr("toolchain", "--install", "gnat_native=1")

# Verify that it appears as available
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("1.0.0") + ".*Available",
             p.out)

# Also that the external compiler is detected and always available
assert_match(".*gnat_external.*Available", p.out)

# Also that the external compiler cannot be uninstalled (and short switch)
p = run_alr("toolchain", "-u", "gnat_external", complain_on_error=False)
assert_match(".*Only regular releases deployed through Alire can be removed.*",
             p.out)

# Verify that it can be uninstalled
run_alr("toolchain", "--uninstall", "gnat_native=1")
p = run_alr("toolchain")
assert "gnat_native" not in p.out, "Unexpected output"

# Repeat install but without giving a version, and one should be autoidentified
# as the newest available version
p = run_alr("toolchain", "-i", "gnat_native", quiet=False)  # Test short switch
assert_match(".*Requested crate resolved as gnat_native=2.0.0.*",
             p.out)

# Verify that we can explicitly install a second version for the same target
run_alr("toolchain", "--install", "gnat_native=1")
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("1.0.0") + ".*Available",
             p.out)

# Verify that uninstalling without specifying version isn't allowed when there
# are two matching crates installed.
p = run_alr("toolchain", "--uninstall", "gnat_native",
            complain_on_error=False)
assert_match(".*Requested crate has several installed releases.*",
             p.out)

# Uninstall successfully by giving a version
run_alr("toolchain", "--uninstall", "gnat_native=2")
# Now we can uninstall without specifying the version of the remaining release
run_alr("toolchain", "--uninstall", "gnat_native")

print('SUCCESS')
