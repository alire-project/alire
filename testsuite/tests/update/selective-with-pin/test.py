"""
A version pin changed in the manifest is applied by `alr update <crate>`, as
pinned crates follow the manifest and do not count as other crates changing.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_versions,
                         alr_pin, alr_unpin, alr_with, init_local_crate,
                         run_alr, use_index_with_manifests)
from drivers.asserts import assert_eq

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0")
add_release_to_index(index, "libbar", "1.1.0")

# Initial solution with version pin

init_local_crate()
alr_with("libfoo")
alr_with("libbar")
alr_pin("libbar", version="1.0.0")
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0"}, alr_lockfile_versions())

# Simulate index update and repin

add_release_to_index(index, "libfoo", "1.1.0")
alr_unpin("libbar", update=False)
alr_pin("libbar", version="1.1.0", update=False)

# The pin is applied without error

run_alr("update", "libfoo")
assert_eq({"libfoo": "1.1.0", "libbar": "1.1.0"}, alr_lockfile_versions())

print("SUCCESS")
