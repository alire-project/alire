"""
`alr update <crate>` updates only the given crate, keeping the versions of the
rest without turning them into direct dependencies, and applies pending
manual edits of the manifest without keeping removed dependencies. When other
crates would have to change, it fails without applying anything.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_states,
                         alr_lockfile_versions, alr_with, init_local_crate,
                         run_alr, use_index_with_manifests)
from drivers.asserts import assert_eq, assert_substring

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")
add_release_to_index(index, "libbaz", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0", depends_on={"libbaz": "^1"})

init_local_crate()
alr_with("libfoo")
alr_with("libbar")

# Generate index updates

add_release_to_index(index, "libfoo", "1.1.0")
add_release_to_index(index, "libbaz", "1.1.0")
add_release_to_index(index, "libbar", "1.1.0", depends_on={"libbaz": "^1"})

# Update single dependency and observe proper behavior (no other updates)

run_alr("update", "libfoo")
assert_eq({"libfoo": "1.1.0", "libbar": "1.0.0", "libbaz": "1.0.0"},
          alr_lockfile_versions())
assert_eq({"libfoo": "direct", "libbar": "direct", "libbaz": "indirect"},
          {crate: state["transitivity"]
           for crate, state in alr_lockfile_states().items()})

# A dependency removed by hand is dropped by the next selective update

alr_with("libbar", delete=True, update=False)
run_alr("update", "libfoo")
assert_eq({"libfoo": "1.1.0"}, alr_lockfile_versions())

# A manual edit requires a newer libbaz, which only happens when requested

alr_with("libbaz=1.0.0")
add_release_to_index(index, "libfoo", "1.2.0")
alr_with("libbaz", delete=True, update=False)
alr_with("libbaz^1.1", update=False)

p = run_alr("update", "libfoo", quiet=False, complain_on_error=False)
assert p.status != 0, f"Unexpected success: {p.out}"
assert_substring("The solution would also change libbaz.", p.out)
assert_eq({"libfoo": "1.1.0", "libbaz": "1.0.0"}, alr_lockfile_versions())

# Add libbaz to the command-line and it succeeds

run_alr("update", "libfoo", "libbaz")
assert_eq({"libfoo": "1.2.0", "libbaz": "1.1.0"}, alr_lockfile_versions())

print("SUCCESS")
