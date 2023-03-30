"""
Check inside a pristine environment that the default cache is located where
it should.
"""

import os

from drivers.alr import alr_with, init_local_crate

# Forcing the deployment of a binary crate triggers the use of the global
# cache, which should be created at the expected location.
init_local_crate()
alr_with("gnat_native")

home = os.environ["HOME"]

assert \
    os.path.isdir(f"{home}/.cache/alire/dependencies/gnat_native_8888.0.0_99fa3a55"), \
    "Default cache not found at the expected location"


print('SUCCESS')
