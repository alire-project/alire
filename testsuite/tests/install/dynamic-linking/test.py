"""
Test installation of dynamically linked executables
"""

from drivers.alr import run_alr, init_local_crate, alr_with, alr_manifest
from drivers.asserts import assert_installed
from drivers.helpers import replace_in_file, on_windows

import os

PREFIX=os.path.join(os.getcwd(), "install")
PREFIX_ARG=f"--prefix={PREFIX}"

# Init a binary crate, add a dependency, and with dynamic linking both should
# appear as installed

os.environ["LIBRARY_TYPE"] = "relocatable"

init_local_crate()
init_local_crate(name="dep", binary=False, enter=False)
alr_with("dep", path="dep")
run_alr("install", PREFIX_ARG, "--this")
assert_installed(PREFIX, ["dep=0.1.0-dev", "xxx=0.1.0-dev"])

# Installing a second binary with the same dependency should be OK

init_local_crate(name="yyy")
alr_with("dep", path="../dep")  # portable paths required for pins
run_alr("install", PREFIX_ARG, "--this")
assert_installed(PREFIX, ["dep=0.1.0-dev", 
                          "xxx=0.1.0-dev",
                          "yyy=0.1.0-dev"])

# Installing a second library version should be OK (not on Windows)
# gprinstall for Windows do not uses Library_Version to create e.g.
# libBlah.so.1 -> libBlah.dll.1. All libraries are simply libBlah.dll

if not on_windows():
    os.chdir("..")
    replace_in_file(os.path.join("dep", alr_manifest()),
                    "0.1.0-dev", "0.2.0")
    init_local_crate(name="zzz")
    alr_with("dep", path="../dep")
    run_alr("install", PREFIX_ARG, "--this")
    assert_installed(PREFIX, ["dep=0.1.0-dev", 
                            "dep=0.2.0", 
                            "xxx=0.1.0-dev",
                            "yyy=0.1.0-dev",
                            "zzz=0.1.0-dev"
                            ])

print('SUCCESS')

