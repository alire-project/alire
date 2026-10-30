"""
Pinning a crate to a local or remote folder solves the dependencies it brings
in, keeping the versions of the existing ones.
"""

import os

from drivers.alr import (add_release_to_index, alr_lockfile_states,
                         alr_lockfile_versions, alr_with, init_local_crate,
                         run_alr, use_index_with_manifests)
from drivers.asserts import assert_eq
from drivers.helpers import init_git_repo

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0")
add_release_to_index(index, "libbaz", "1.0.0")

# Crates to be pinned, each bringing in a dependency
for crate, dep in [("linked", "libbar"), ("remote", "libbaz")]:
    init_local_crate(name=crate, enter=False)
    alr_with(dep, update=False, in_crate=crate)
remote_url = os.path.join(os.getcwd(), "remote")
init_git_repo("remote")

init_local_crate()
alr_with("libfoo")
add_release_to_index(index, "libfoo", "1.1.0")

# Pin (link) and check no unwanted updates

run_alr("with", "linked", "--use", os.path.join("..", "linked"))
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0"}, alr_lockfile_versions())
assert_eq("linked", alr_lockfile_states()["linked"]["fulfilment"])

# Likewise for "remote" pin

run_alr("with", "remote", "--use", remote_url)
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0", "libbaz": "1.0.0"},
          alr_lockfile_versions())
assert_eq("linked", alr_lockfile_states()["remote"]["fulfilment"])

print("SUCCESS")
