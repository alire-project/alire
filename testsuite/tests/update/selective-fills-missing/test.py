"""
`alr update <crate>` fills in dependencies that were missing, when it can do
so without changing other crates. IOWs, missing dependencies are filled at
the first opportunity and not kept unsolved forcibly.
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
alr_with("libbar", force=True)  # Not in the index yet

# Simulate index updates

add_release_to_index(index, "libfoo", "1.1.0")
add_release_to_index(index, "libqux", "1.1.0")
add_release_to_index(index, "libbar", "1.0.0")

# After the targeted update, other dependencies have been filled too.
# This is shown in the feedback message so the user would be aware. 
# Not shown in the test.

run_alr("update", "libfoo")
assert_eq({"libfoo": "1.1.0", "libqux": "1.0.0", "libbar": "1.0.0"},
          alr_lockfile_versions())

print("SUCCESS")
