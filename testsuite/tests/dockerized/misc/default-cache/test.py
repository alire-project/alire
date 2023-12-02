"""
Check inside a pristine environment that the default cache is located where
it should.
"""

import os

from drivers import builds
from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.helpers import contents

# Forcing the deployment of a binary crate triggers the use of the global
# cache, which should be created at the expected location.
init_local_crate()
alr_with("gnat_native")

home = os.environ["HOME"]

base = f"{home}/.local/share/alire"

assert \
    os.path.isdir(f"{base}/toolchains/gnat_native_8888.0.0_99fa3a55"), \
    f"Toolchain dir not found at the expected location: {contents(base)}"

# Let's also check the rest of dirs for shared builds

# First, prevent an attempt at downloading a real compiler
run_alr("toolchain", "--disable-assistant")

builds.enable_shared()  # Enabled here as we are using the Docker driver
alr_with("crate_real")  # This release will go in the cache

# Read-only vault
assert \
    os.path.isdir(f"{base}/releases/crate_real_1.0.0_filesystem"), \
    f"Vault not found at the expected location: f{contents(base)}"

# Shared builds

# This generates the synced build dir. It fails because there is no toolchain
# configured, but that is not relevant for this test.
run_alr("build", complain_on_error=False)

# We hardcode this hash so we detect unwilling changes to our hashing scheme.
# Every time this hash changes we must know the reason (changes in the hashing
# procedures)
hash = "0774083df8ff003084c32cabdec6090a58b41c6be317cec0475df5eacbca0d23"
assert \
    os.path.isdir(f"{base}/builds/crate_real_1.0.0_filesystem_{hash}"), \
    f"Shared build not found at the expected location: f{contents(base)}"

print('SUCCESS')
