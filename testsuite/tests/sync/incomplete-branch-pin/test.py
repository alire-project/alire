"""
When dependencies cannot be satisfied, `alr sync` points out that pins
tracking a branch are only refreshed by `alr update`, but only when there are
such pins.
"""

import os

from drivers.alr import (alr_pin, alr_unpin, alr_with, init_local_crate,
                         run_alr, use_index_with_manifests)
from drivers.asserts import assert_substring
from drivers.helpers import init_git_repo

INCOMPLETE = ("Workspace synchronized, but some dependencies cannot be "
              "satisfied by the available releases.")
PIN_NOTE = "Pins tracking a branch are only refreshed by `alr update`"

use_index_with_manifests({})

# A remote crate depending on a crate that is not in the index
init_local_crate(name="remote", enter=False)
with open(os.path.join("remote", "alire.toml"), "a") as f:
    f.write('\n[[depends-on]]\nlibnope = "*"\n')
url = os.path.join(os.getcwd(), "remote")
head = init_git_repo("remote")

init_local_crate()
alr_with("remote", update=False)

# Pinned to a branch, the note is shown
alr_pin("remote", url=url, branch="master", update=False)
p = run_alr("sync", quiet=False)
assert_substring(INCOMPLETE, p.out)
assert_substring(PIN_NOTE, p.out)

# Pinned to a commit, it is not
alr_unpin("remote", update=False)
alr_pin("remote", url=url, commit=head, update=False)
p = run_alr("sync", quiet=False)
assert_substring(INCOMPLETE, p.out)
assert PIN_NOTE not in p.out, f"Unexpected note about pins: {p.out}"

print("SUCCESS")
