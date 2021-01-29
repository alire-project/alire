"""
Check behavior of unknown enum values in manifests
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import glob
import os

# Verify that we can list the index, despite containing an unknown distro value
run_alr("search", "--crates")

# Verify that checking the index strictly does fail
p = run_alr("index", "--check", complain_on_error=False)
assert_match(".*invalid enumeration value:.*", p.out)

# Verify that we can retrieve and load such a crate's manifest
run_alr("get", "crate")
os.chdir(glob.glob("crate_*")[0])
run_alr("show")

# Verify that adding bad values to our local manifest won't slip by
with open("alire.toml", "at") as manifest:
    manifest.write("""
[available.'case(distribution)']
ubuntu = true
nonexistent-distro = false\n""")
p = run_alr("show", complain_on_error=False)
assert_match(".*invalid enumeration value:.*", p.out)

print('SUCCESS')
