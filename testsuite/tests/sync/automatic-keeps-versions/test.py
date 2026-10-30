"""
The automatic sync after a manual edit of the manifest keeps the existing
solution, as `alr sync` does, instead of updating every dependency.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_versions,
                         alr_with, init_local_crate, run_alr,
                         use_index_with_manifests)
from drivers.asserts import assert_eq, assert_substring

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0")

init_local_crate()
alr_with("libfoo")

# Newer releases appear, and an unrelated dependency is added by hand
add_release_to_index(index, "libfoo", "1.1.0")
alr_with("libbar", update=False)

# Any command in the workspace applies the edit without updating
p = run_alr("with", quiet=False)
assert_substring("Dependencies synchronized as follows:", p.out)
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0"}, alr_lockfile_versions())

# An update brings in the new release
run_alr("update")
assert_eq({"libfoo": "1.1.0", "libbar": "1.0.0"}, alr_lockfile_versions())

print("SUCCESS")
