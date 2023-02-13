"""
Test installation of statically linked executables
"""

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_installed

import os

PREFIX=os.path.join(os.getcwd(), "install")
PREFIX_ARG=f"--prefix={PREFIX}"

# Init a binary crate, add a dependency, and by default with static linking
# only the root dependency should show as installed

os.environ["LIBRARY_TYPE"] = "static" # It's the default, but just in case

init_local_crate()
init_local_crate(name="dep", binary=False, enter=False)
alr_with("dep", path="dep")
run_alr("install", PREFIX_ARG)
assert_installed(PREFIX, ["xxx=0.1.0-dev"])

print('SUCCESS')
