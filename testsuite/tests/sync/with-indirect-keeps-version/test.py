"""
`alr with` on a crate that is already an indirect dependency narrows the new
dependency to the version in use, rather than to the newest release.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_states,
                         alr_lockfile_versions, alr_manifest, alr_with,
                         init_local_crate, run_alr, use_index_with_manifests)
from drivers.asserts import assert_eq, assert_substring
from drivers.helpers import content_of

index = use_index_with_manifests({})
add_release_to_index(index, "libbaz", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0",
                     depends_on={"libbaz": "^1"})

init_local_crate()
alr_with("libbar")

# Add an update to the index, and make that a direct dependency

add_release_to_index(index, "libbaz", "1.1.0")
run_alr("with", "libbaz")

# Verify the indirect version is still in use as direct dependency

assert_substring('libbaz = "^1.0.0"', content_of(alr_manifest()))
assert_eq({"libbar": "1.0.0", "libbaz": "1.0.0"}, alr_lockfile_versions())
assert_eq("direct", alr_lockfile_states()["libbaz"]["transitivity"])

print("SUCCESS")
