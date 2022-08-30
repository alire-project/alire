"""
Verify that pins when there is no lockfile are correctly applied on first run
"""

from drivers.alr import run_alr, alr_pin, init_local_crate, alr_lockfile
from drivers.asserts import assert_eq

import os


fake_dep = "unobtainium"

# Create a crate
init_local_crate()

# Add a dependency
run_alr("with", fake_dep, force=True)

# Pin to a local folder
os.mkdir(fake_dep)
alr_pin(fake_dep, path=fake_dep)

# Remove the lockfile
os.remove(alr_lockfile())

# Check that the pin is applied on next command run
p = run_alr("pin")
assert_eq(f"{fake_dep} file:{fake_dep}\n", p.out)


print('SUCCESS')
