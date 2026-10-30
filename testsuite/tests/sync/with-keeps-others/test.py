"""
`alr with` adds a dependency keeping the versions of the existing ones, even
when newer releases are available.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_versions,
                         alr_with, init_local_crate, run_alr,
                         use_index_with_manifests)
from drivers.asserts import assert_eq

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")

init_local_crate()
alr_with("libfoo")

# New releases in the index, including one update

add_release_to_index(index, "libfoo", "1.1.0")
add_release_to_index(index, "libbar", "1.0.0")

# New dependency doesn't cause update

run_alr("with", "libbar")
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0"}, alr_lockfile_versions())

# Likewise for removal

run_alr("with", "--del", "libbar")
assert_eq({"libfoo": "1.0.0"}, alr_lockfile_versions())

print("SUCCESS")
