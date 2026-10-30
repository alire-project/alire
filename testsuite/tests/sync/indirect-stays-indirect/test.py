"""
`alr sync` keeps the previous solution without turning indirect dependencies
into direct ones.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_states,
                         alr_touch_manifest, alr_with, init_local_crate,
                         run_alr, use_index_with_manifests)
from drivers.asserts import assert_eq

index = use_index_with_manifests({})
add_release_to_index(index, "libbaz", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0",
                     depends_on={"libbaz": "^1"})

init_local_crate()
alr_with("libbar")

# Sync in presence of new version

add_release_to_index(index, "libbaz", "1.1.0")
alr_touch_manifest()
run_alr("sync")

# The new version is not used and it stays indirect

states = alr_lockfile_states()
assert_eq({"libbar": ("direct", "*", "1.0.0"),
           "libbaz": ("indirect", "^1", "1.0.0")},
          {crate: (state["transitivity"], state["versions"],
                   state["release"]["version"])
           for crate, state in states.items()})

print("SUCCESS")
