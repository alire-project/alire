"""
Verify that paths given in XDG_* environment variables are scrubbed for
trailing slashes.
"""

import os

from drivers.alr import run_alr, version_info
from drivers.asserts import assert_eq


run_alr("version") # prevent spurious output on first run

fake = os.path.join(os.getcwd(), "FAKE")
fake_settings = os.path.join(fake, "alire")

# Pre-configure the settings that will be found at the fake location,
# so no auto-configuration of the community index is attempted later.

os.makedirs(fake_settings)
run_alr("-s", fake_settings, "settings", "--global",
        "--set", "index.auto_community", "false")

# Point the XDG variables used by Alire to the fake dir, with extra
# trailing slashes that formerly resulted in an invalid path error.
# ALIRE_SETTINGS_DIR must be removed so the settings location (and in
# turn the cache location) is derived from the XDG variables.

del os.environ["ALIRE_SETTINGS_DIR"]
os.environ["XDG_CONFIG_HOME"] = fake + "///"
os.environ["XDG_DATA_HOME"] = fake + "///"
os.environ["XDG_RUNTIME_DIR"] = fake + "///"

info = version_info()

# Settings and cache folders are <XDG var>/alire, once properly scrubbed

assert_eq(fake_settings, info["settings folder"])
assert_eq(fake_settings, info["cache folder"])

# The temp folder is XDG_RUNTIME_DIR itself; it undergoes further
# normalization, so we only check that it has been scrubbed

temp = info["temp folder"]
assert "//" not in temp and not temp.endswith("/"), \
    f"temp folder not scrubbed: {temp}"

print("SUCCESS")
