"""
`alr sync` moves a dependency whose constraint no longer admits the version in
the solution to the newest admitted one, and keeps the rest of the solution.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_versions,
                         alr_with, init_local_crate, run_alr,
                         use_index_with_manifests)
from drivers.asserts import assert_eq

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0")

init_local_crate()
alr_with("libfoo^1")
alr_with("libbar")

for version in ["1.1.0", "2.0.0", "2.1.0"]:
    add_release_to_index(index, "libfoo", version)
add_release_to_index(index, "libbar", "1.1.0")

alr_with("libfoo", delete=True, update=False)
alr_with("libfoo^2", update=False)
run_alr("sync")

# libfoo is updated due to dependency but libbar stays untouched
assert_eq({"libfoo": "2.1.0", "libbar": "1.0.0"}, alr_lockfile_versions())

print("SUCCESS")
