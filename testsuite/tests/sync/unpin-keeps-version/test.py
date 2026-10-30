"""
After removing a version pin from the manifest, `alr sync` keeps the pinned
version, as it is in the index, rather than moving to the newest one.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_states,
                         alr_lockfile_versions, alr_pin, alr_unpin, alr_with,
                         init_local_crate, run_alr, use_index_with_manifests)
from drivers.asserts import assert_eq

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")
add_release_to_index(index, "libfoo", "1.1.0")

init_local_crate()
alr_with("libfoo")
alr_pin("libfoo", version="1.0.0")
assert_eq({"libfoo": "1.0.0"}, alr_lockfile_versions())
assert alr_lockfile_states()["libfoo"]["pinned"]

# Rather than updating the freed pin, the version is kept

alr_unpin("libfoo", update=False)
run_alr("sync")
assert_eq({"libfoo": "1.0.0"}, alr_lockfile_versions())
assert not alr_lockfile_states()["libfoo"]["pinned"]

print("SUCCESS")
