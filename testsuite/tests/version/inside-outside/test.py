"""
Verify that `alr version` works without error without, with and with broken
manifest
"""

from drivers.alr import run_alr, init_local_crate, alr_manifest
from drivers.asserts import assert_eq, assert_match

# Run in several situations with minimal double-check

p = run_alr("version")
assert_match(".*root status:.*OUTSIDE", p.out)

init_local_crate()
p = run_alr("version")
assert_match(".*root status:.*VALID", p.out)

# break the manifest
with open(alr_manifest(), "a") as manifest:
    manifest.write("not valid TOML methinks...")
p = run_alr("version")
assert_match(".*root status:.*BROKEN", p.out)

print('SUCCESS')
