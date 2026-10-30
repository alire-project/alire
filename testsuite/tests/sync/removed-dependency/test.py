"""
`alr sync` drops a dependency removed from the manifest, together with the
indirect dependencies only it needed, and keeps the rest of the solution.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_versions,
                         alr_with, init_local_crate, run_alr,
                         use_index_with_manifests)
from drivers.asserts import assert_eq

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")
add_release_to_index(index, "libbaz", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0",
                     depends_on={"libbaz": "^1"})

# Expected initial solution

init_local_crate()
alr_with("libfoo")
alr_with("libbar")
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0", "libbaz": "1.0.0"},
          alr_lockfile_versions())

# No unwanted update nor now unneeded indirect dependencies

add_release_to_index(index, "libfoo", "1.1.0")
alr_with("libbar", delete=True, update=False)
run_alr("sync")
assert_eq({"libfoo": "1.0.0"}, alr_lockfile_versions())

print("SUCCESS")
