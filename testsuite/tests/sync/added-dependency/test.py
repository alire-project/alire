"""
`alr sync` solves a dependency added to the manifest, changing in the existing
solution only what the new dependency requires.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_versions,
                         alr_with, init_local_crate, run_alr,
                         use_index_with_manifests)
from drivers.asserts import assert_eq

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")
add_release_to_index(index, "libqux", "1.0.0")

init_local_crate()
alr_with("libfoo")
alr_with("libqux")

# The new dependency needs a newer libfoo; libqux has also a newer release
add_release_to_index(index, "libfoo", "1.1.0")
add_release_to_index(index, "libfoo", "1.2.0")
add_release_to_index(index, "libbar", "1.0.0",
                     depends_on={"libfoo": "~1.1"})
add_release_to_index(index, "libqux", "1.1.0")

alr_with("libbar", update=False)
run_alr("sync")
assert_eq({"libfoo": "1.1.0", "libbar": "1.0.0", "libqux": "1.0.0"},
          alr_lockfile_versions())

print("SUCCESS")
