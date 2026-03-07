"""
Verify that `alr` doesn't depend on anything being on `PATH` for basic commands
(e.g. `.dll` files on Windows).
"""


import os
from drivers.alr import run_alr


# Check `alr --version` succeeds and prints something.
p = run_alr("--version")
assert len(p.out) > 0

# Completely clear `PATH`.
#
# Setting it to an empty string causes a buffer overflow on some platforms
# so we set it to a nonexistent directory.
os.environ["PATH"] = os.path.join(os.getcwd(), "nonexistent")

# Check `alr --version` still succeeds and prints something.
p = run_alr("--version")
assert len(p.out) > 0


print("SUCCESS")
