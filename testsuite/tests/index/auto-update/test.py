"""
Check that index auto-update works as expected
"""

import os
from drivers.alr import run_alr
from drivers.helpers import content_of, lines_of

CONFIG_FILE = os.path.join("alr-config", "settings.toml")

# First, we check no related configuration exists

assert "auto_update_asked = true\n" not in lines_of(CONFIG_FILE), \
    "UNEXPECTED:\n" + content_of(CONFIG_FILE)

# Auto updates are disabled by default by the python harness, to maintain
# compatibility of older tests. We disable the disabling now.

run_alr("settings", "--global", "--unset", "index.auto_update")
# This was set to 0 to disable updates

# After using the index, the user will have been asked

run_alr("search", "hello")

assert "auto_update_asked = true\n" in lines_of(CONFIG_FILE), \
    "UNEXPECTED:\n" + content_of(CONFIG_FILE)

print('SUCCESS')
