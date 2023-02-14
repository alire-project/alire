"""
Test installation to default location
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq
from drivers.helpers import on_windows

import os

# Override HOME to our test location so it is pristine

if on_windows():
    os.environ["USERPROFILE"] = os.getcwd()
else:
    os.environ["HOME"] = os.getcwd()

# Disable msys2 as the home change doesn't sit well with it and we don't need it here.
# This is a workaround because trying to disable it via config doesn't work on 1st run:
# run_alr("config", "--global", "--set", "msys2.do_not_install", "true")
if on_windows():
    os.makedirs(os.path.join(os.environ["USERPROFILE"], ".cache", "alire", "msys64"))

p = run_alr("install", "--info", quiet=False)
assert_eq(f"There is no installation at prefix {os.path.join(os.getcwd(), '.alire')}\n", 
          p.out)

# Install a local crate to check prefix creation and contents at expected place
init_local_crate()
run_alr("install")
os.chdir("..")
p = run_alr("install", "--info", quiet=False)
assert_eq(f"Installation prefix found at {os.path.join(os.getcwd(), '.alire')}\n"
          "Contents:\n"
          "   xxx=0.1.0-dev\n", 
          p.out)


print('SUCCESS')
