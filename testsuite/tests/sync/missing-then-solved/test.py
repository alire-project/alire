"""
`alr sync` solves a dependency that was missing once it becomes available,
even without changes to the manifest, keeping the rest of the solution.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_states,
                         alr_lockfile_versions, alr_with, init_local_crate,
                         run_alr, use_index_with_manifests)
from drivers.asserts import assert_eq, assert_substring

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")

init_local_crate()
alr_with("libfoo")
alr_with("libbar", force=True)  # Not in the index yet
assert_eq("missed", alr_lockfile_states()["libbar"]["fulfilment"])

# While it is missing, the sync warns instead of reporting success
p = run_alr("sync", quiet=False)
assert_substring("Workspace synchronized, but some dependencies cannot be "
                 "satisfied by the available releases.", p.out)
assert "Success" not in p.out, f"Unexpected success: {p.out}"
assert "alr update" not in p.out, f"Unexpected mention of update: {p.out}"

add_release_to_index(index, "libbar", "1.0.0")
add_release_to_index(index, "libfoo", "1.1.0")
run_alr("sync")
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0"}, alr_lockfile_versions())

print("SUCCESS")
