"""
Test for bug #1496, in which a configuration at a non-default location was
loaded too late.
"""

import os
from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

# Create a custom configuration dir + file
custom_config = "custom_config"
os.mkdir(custom_config)
with open(os.path.join(custom_config, "config.toml"), "w") as f:
    f.write("test_value = 42\n")

expected = "test_value=42\n"

# Verify proper loading with both short and long config options
assert_eq(expected,
          run_alr("-c", custom_config, "settings", "--global").out)
assert_eq(expected,
          run_alr(f"--config={custom_config}", "settings", "--global").out)

# Verify also when using environment variable
os.environ["ALIRE_SETTINGS_DIR"] = os.path.abspath(custom_config)
assert_eq(expected,
          run_alr("settings", "--global").out)

print('SUCCESS')
