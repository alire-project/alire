"""
Check basic use: manual install, uninstall, and listing of toolchains
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

# Install a precise version of gnat
run_alr("toolchain", "--install", "gnat_native=7777")

# Verify that it appears as available
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("7777.0.0") + ".*Available",
             p.out)

# Also that the external compiler is detected and always available
assert_match(".*gnat_external.*Available", p.out)

# Also that the external compiler cannot be uninstalled (and short switch)
p = run_alr("toolchain", "-u", "gnat_external", complain_on_error=False)
assert_match(".*Only regular releases deployed through Alire can be removed.*",
             p.out)

# Verify that it can be uninstalled
run_alr("toolchain", "--uninstall", "gnat_native=7777")
p = run_alr("toolchain")
assert "gnat_native" not in p.out, "Unexpected output"

# Repeat install but without giving a version, and one should be autoidentified
# as the newest available version
p = run_alr("toolchain", "-i", "gnat_native", quiet=False)  # Test short switch
assert_match(".*Requested crate resolved as gnat_native=8888.0.0.*",
             p.out)

# Verify that we can explicitly install a second version for the same target
run_alr("toolchain", "--install", "gnat_native=7777")
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("7777.0.0") + ".*Available",
             p.out)

# Verify that uninstalling without specifying version isn't allowed when there
# are two matching crates installed.
p = run_alr("toolchain", "--uninstall", "gnat_native",
            complain_on_error=False)
assert_match(".*Requested crate has several installed releases.*",
             p.out)

# Uninstall successfully by giving a version
run_alr("toolchain", "--uninstall", "gnat_native=8888")
# Now we can uninstall without specifying the version of the remaining release
run_alr("toolchain", "--uninstall", "gnat_native")


# Verify that two components can be installed at once
run_alr("toolchain", "--install", "gnat_native", "gnat_cross_1")
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("8888.0.0") + ".*Available",
             p.out)
assert_match(".*gnat_cross_1.*" + re.escape("9999.0.0") + ".*Available",
             p.out)

# Uninstall both components
run_alr("toolchain", "--uninstall", "gnat_native", "gnat_cross_1")


print('SUCCESS')
