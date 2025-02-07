"""
Verify that when there's a hash mismatch between an installed tool and the one
in our index for the same version, we are able to detect it and give advice on
how to fix the situation.
"""

import os
from drivers.alr import run_alr, alr_settings_dir
from drivers.asserts import assert_substring
from drivers.helpers import content_of

# We can trigger the situation by configuring a toolchain and tampering with
# the hash in the toolchain cache.

run_alr("toolchain", "--select", "gprbuild", "gnat_native")

target_file = os.path.join(alr_settings_dir(),
                           "cache",
                           "toolchains",
                           "gprbuild_1.0.0_e3d52b4a",
                           "alire.toml")

OLD = "e3d52b4a441a56ab1c0175ee8ea407864b72879cf73184a9f7d68eef53a87451"
NEW = "00002b4a441a56ab1c0175ee8ea407864b72879cf73184a9f7d68eef53a87451"

# Modify the hash in the toolchain cache
lines = content_of(target_file)
lines = lines.replace(OLD, NEW)
assert_substring(NEW, lines)

# Replace contents of the target file
with open(target_file, "w") as f:
    f.write(lines)

# Verify the diagnostic message
assert_substring("Selected tool gprbuild=1.0.0 does not match its fingerprint from the index",
                 run_alr("toolchain", complain_on_error=False).out)

print("SUCCESS")
