"""
Verify that distro overridding works as intended
"""

from drivers.alr import run_alr, distro_is_known
from drivers.asserts import assert_match

# Overriding distro detection. We force Debian as our tests run in Ubuntu and
# many other distros so it is only giving a false positive on Debian.
run_alr("settings", "--global",
        "--set", "distribution.override", "debian")

assert_match(".*distribution:[^\n]*DEBIAN",
             run_alr("version").out)

# Disabling distro detection takes precedence
run_alr("settings", "--global",
        "--set", "distribution.disable_detection", "true")

assert_match(".*distribution:[^\n]*DISTRIBUTION_UNKNOWN",
             run_alr("version").out)

print('SUCCESS')
