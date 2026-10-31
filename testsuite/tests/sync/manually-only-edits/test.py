"""
Under update.manually_only, manual edits of the manifest are applied as long
as no release already in the solution has to change. Otherwise, commands fail
without changing anything, until the change is requested with `alr update`.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_versions,
                         alr_settings_set, alr_with, init_local_crate,
                         run_alr, use_index_with_manifests)
from drivers.asserts import assert_eq, assert_substring

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0")

# Add first dependency and enable manual-only updates

init_local_crate()
alr_with("libfoo")
alr_settings_set("update.manually_only", "true")

# A dependency added by hand is applied, keeping the rest
# at the previous version even though an update exists

add_release_to_index(index, "libfoo", "1.1.0")
alr_with("libbar", update=False)
run_alr("with")
assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0"}, alr_lockfile_versions())

# An edit that needs a newer libfoo fails, both automatically and on sync

alr_with("libfoo", delete=True, update=False)
alr_with("libfoo^1.1", update=False)
for command in ["with", "sync"]:
    p = run_alr(command, quiet=False, complain_on_error=False)
    assert p.status != 0, f"Unexpected success of {command}: {p.out}"
    assert_substring(
        "ERROR: Dependencies cannot change while `update.manually_only` is"
        " set:\n"
        "ERROR:    Applying the manifest would change libfoo. Run"
        " `alr update libfoo`, or `alr update` for all dependencies, to apply"
        " it; or adjust the manifest.\n",
        p.out)
    assert_eq({"libfoo": "1.0.0", "libbar": "1.0.0"}, alr_lockfile_versions())

# Requesting the update applies it, as intended

run_alr("update", "libfoo")
assert_eq({"libfoo": "1.1.0", "libbar": "1.0.0"}, alr_lockfile_versions())

print("SUCCESS")
