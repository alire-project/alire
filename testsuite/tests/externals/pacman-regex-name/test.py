"""
pacman -Ss takes a regex, so system package names with special characters
(e.g. "gtk+3") must be escaped to be found. Checked with a mocked pacman that
only finds the package when searched with the properly escaped regex.
"""

from drivers.alr import alr_settings_set, run_alr
from drivers.asserts import assert_match
from drivers.helpers import MockCommand

# Answer like `pacman -Ss` only for the exact, escaped, search for gtk+3
MOCK_PACMAN = """\
import sys

if sys.argv[1:] == ["-Ss", r"^gtk\\+3$"]:
    print("extra/gtk+3 3.24.43-1")
    print("    GObject-based multi-platform GUI toolkit")
    sys.exit(0)
sys.exit(1)
"""

# Use pacman whatever the actual distribution
alr_settings_set("distribution.disable_detection", "false")
alr_settings_set("distribution.override", "arch")

with MockCommand("pacman", MOCK_PACMAN, "mocks"):
    p = run_alr("show", "ext", "--external-detect")

assert_match(r".*ext=3\.24\.43", p.out)

print("SUCCESS")
