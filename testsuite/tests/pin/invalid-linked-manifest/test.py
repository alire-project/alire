"""
https://github.com/alire-project/alire/issues/1822
Check that a link to a suddenly invalid manifest is not silently ignored
"""

from drivers.alr import alr_pin, init_local_crate, run_alr
from drivers.asserts import assert_substring

# We start by linking to a crate with a valid manifest

LINKED = "linked"

init_local_crate()
init_local_crate(LINKED, enter=False)
alr_pin(LINKED, path=LINKED)

# Verify that the crate is linked as expected
assert_substring(f"{LINKED} file:{LINKED}", run_alr("pin").out)

# Invalidate the manifest
with open(f"{LINKED}/alire.toml", "w") as f:
    f.write("nope")

# Verify that updating fails
p = run_alr("update", complain_on_error=False)

# Check the error message
assert_substring("Invalid manifest in pinned crate at", p.out)

print("SUCCESS")