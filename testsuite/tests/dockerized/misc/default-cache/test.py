"""
Check inside a pristine environment that the default cache is located where
it should.
"""

import os

from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.helpers import contents

# Forcing the deployment of a binary crate triggers the use of the global
# cache, which should be created at the expected location.
init_local_crate()
alr_with("gnat_native")

home = os.environ["HOME"]

base = f"{home}/.cache/alire"

assert \
    os.path.isdir(f"{base}/toolchains/gnat_native_8888.0.0_99fa3a55"), \
    f"Toolchain dir not found at the expected location: {contents(base)}"

# Let's also check the rest of dirs for shared builds

# First, prevent an attempt at downloading a real compiler
run_alr("toolchain", "--disable-assistant")

run_alr("config", "--global", "--set", "dependencies.shared", "true")
alr_with("crate_real")  # This release will go in the cache

# Read-only vault
assert \
    os.path.isdir(f"{base}/releases/crate_real_1.0.0_filesystem"), \
    f"Vault not found at the expected location: f{contents(base)}"

# Shared builds
# We hardcode this hash so we detect unwilling changes to our hashing scheme
hash = "e66592c9a181de97dc3a342cf76378f6ffa6667d7c1864c74d98bec8ffaf4f3d"
assert \
    os.path.isdir(f"{base}/builds/crate_real_1.0.0_filesystem_{hash}"), \
    f"Shared build not found at the expected location: f{contents(base)}"

print('SUCCESS')
