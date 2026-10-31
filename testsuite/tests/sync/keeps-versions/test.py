"""
`alr sync` applies manifest changes keeping the existing solution, even when
newer releases are available, which only `alr update` brings in.
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

p = run_alr("sync", quiet=False)
assert_substring("libbar 1.0.0 (new)", p.out)
assert "libfoo" not in p.out, f"Unexpected change to libfoo: {p.out}"
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0"}, alr_lockfile_versions())

# A sync without changes in the manifest still keeps the solution
run_alr("sync")
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0"}, alr_lockfile_versions())

# An update brings in the new release
run_alr("update")
assert_eq({"libfoo": "1.1.0", "libbar": "1.0.0"}, alr_lockfile_versions())

print("SUCCESS")
