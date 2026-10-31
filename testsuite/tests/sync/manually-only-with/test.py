"""
Under update.manually_only, `alr with` fails when the new dependency needs a
release already in the solution to change; making the change by hand and
running `alr update` for that release applies it. Pinning, being an explicit
request, may change a release.
"""

from drivers.alr import (add_release_to_index, alr_lockfile_versions,
                         alr_manifest, alr_settings_set, alr_with,
                         init_local_crate, run_alr, use_index_with_manifests)
from drivers.asserts import assert_eq, assert_substring
from drivers.helpers import content_of

index = use_index_with_manifests({})
add_release_to_index(index, "libfoo", "1.0.0")

init_local_crate()
alr_with("libfoo")
alr_settings_set("update.manually_only", "true")

# Add some updates to the index, that also require updates in deps

add_release_to_index(index, "libfoo", "1.1.0")
add_release_to_index(index, "libfoo", "2.0.0")
add_release_to_index(index, "libqux", "1.0.0")
add_release_to_index(index, "libbar", "1.0.0", depends_on={"libfoo": "~1.1"})

# A dependency that fits the current solution is added, no problem

run_alr("with", "libqux")
assert_eq({"libfoo": "1.0.0", "libqux": "1.0.0"}, alr_lockfile_versions())

# One that needs a newer libfoo is rejected, without changes

p = run_alr("with", "libbar", quiet=False, complain_on_error=False)
assert p.status != 0, f"Unexpected success: {p.out}"
assert_substring("The requested change would also change libfoo, so it was "
                 "not applied.", p.out)
assert "libbar" not in content_of(alr_manifest()), "Unexpected manifest edit"
assert_eq({"libfoo": "1.0.0", "libqux": "1.0.0"}, alr_lockfile_versions())

# Following the advice applies it, with libfoo within what libbar needs

alr_with("libbar", update=False)
run_alr("update", "libfoo")
assert_eq({"libfoo": "1.1.0", "libqux": "1.0.0", "libbar": "1.0.0"},
          alr_lockfile_versions())

# A pin may change a release, as it is explicitly requested

add_release_to_index(index, "libqux", "1.1.0")
run_alr("pin", "libqux=1.1.0")
assert_eq({"libfoo": "1.1.0", "libqux": "1.1.0", "libbar": "1.0.0"},
          alr_lockfile_versions())

print("SUCCESS")
