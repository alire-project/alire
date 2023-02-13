"""
Test installation of executables in dependencies
"""

from drivers.alr import run_alr, init_local_crate, alr_with, alr_manifest
from drivers.asserts import assert_contents, assert_installed, assert_match
from drivers.helpers import on_windows, replace_in_file

import os

START=os.getcwd()
PREFIX=os.path.join(os.getcwd(), "install")
PREFIX_ARG=f"--prefix={PREFIX}"

# Init a binary crate, add an executable dependency, and both should
# end being installed.

os.environ["LIBRARY_TYPE"] = "static" # It's the default, but just in case

init_local_crate()
init_local_crate(name="dep", binary=True, enter=False)
alr_with("dep", path="dep")
run_alr("install", PREFIX_ARG)

# Check gprbuild manifests
assert_installed(PREFIX, ["dep=0.1.0-dev",
                          "xxx=0.1.0-dev"])

ext = (".exe" if on_windows() else "")

# Check actual executables in place
os.chdir(os.path.join("..", "install", "bin"))  # simplifies paths in check
assert_contents(".", 
                [f"./dep{ext}",
                 f"./xxx{ext}"])

# Attempting to install a different version of the dependency should fail,
# since only a single version of an executable can live in a prefix

os.chdir(START)

replace_in_file(os.path.join("xxx", "dep", alr_manifest()),
                "0.1.0-dev", "0.2.0")
init_local_crate(name="yyy")
alr_with("dep", path="../xxx/dep")
p = run_alr("install", PREFIX_ARG, complain_on_error=False)
assert_match(".*Release dep=0.2.0 conflicts with already installed dep=0.1.0-dev.*",
             p.out)

print("SUCCESS")