"""
Package managers receive system package names after "--", so that a name can
never be taken as an option (names are also validated when loading, see
tests/index/bad-system-package-name). Checked with the Debian tools, mocked to
record their arguments.
"""

import json
import os

from drivers.alr import alr_settings_set, run_alr
from drivers.helpers import MockCommand

LOG = os.path.abspath("calls.log")

# Record the arguments, and report the package as not found
MOCK_SCRIPT = f"""\
import json
import os
import sys

with open({LOG!r}, "a") as f:
    f.write(json.dumps([os.path.basename(sys.argv[0])] + sys.argv[1:]) + "\\n")
sys.exit({{exit_code}})
"""

# Use the Debian tools whatever the actual distribution
alr_settings_set("distribution.disable_detection", "false")
alr_settings_set("distribution.override", "debian")

with MockCommand("dpkg", MOCK_SCRIPT.format(exit_code=1), "mocks"), \
     MockCommand("apt-cache", MOCK_SCRIPT.format(exit_code=100), "mocks"):
    run_alr("show", "ext", "--external-detect")

with open(LOG) as f:
    calls = [json.loads(line)[1:] for line in f]

assert calls, "No package manager was called"
for args in calls:
    assert args[-2:] == ["--", "libext-dev"], \
        f"Package name not preceded by '--' in: {args}"

print("SUCCESS")
