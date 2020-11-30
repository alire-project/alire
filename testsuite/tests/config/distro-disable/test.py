"""
Verify that disabling distro detection works as intended
"""

import os

from glob import glob

from drivers.alr import run_alr, distro_is_known

run_alr("config", "--global",
        "--set", "distribution.disable_detection", "true")

assert not distro_is_known(), "Unexpected distro detection"

print('SUCCESS')
