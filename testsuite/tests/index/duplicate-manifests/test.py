"""
Releases of a crate whose versions are equal once build metadata is ignored
(as semantic versioning mandates) are duplicates. Otherwise, whichever
manifest loaded first would silently shadow the other. All manifests in a
duplicate group are rejected, regardless of load order. Leading zeros cannot
create duplicates, as such versions are rejected by themselves.
"""

import re

from drivers.alr import run_alr, use_index_with_manifests
from drivers.asserts import (assert_invalid_manifest, assert_match,
                             assert_not_substring)

MANIFEST = """\
name = "crate"
version = "{version}"
description = "Test crate"
maintainers = ["example@example.com"]
maintainers-logins = ["mylogin"]

[origin]
url = "file:."
"""


def index_with(*versions):
    use_index_with_manifests(
        {f"crate-{v}.toml": MANIFEST.format(version=v) for v in versions})


def check_duplicates(*versions):
    """
    Check that all releases with the given versions are rejected
    """
    index_with(*versions)
    assert_invalid_manifest(r"Duplicate version .* also in: ", "show", "crate")

    # Every manifest in the group is reported, not just the one loading first
    p = run_alr("show", "crate", quiet=False, complain_on_error=False)
    for version in versions:
        assert_match(".*Warning: Skipping invalid index manifest .*"
                     + re.escape(f"crate-{version}.toml"), p.out)


def check_no_duplicates(*versions):
    """
    Check that releases with the given versions all load fine
    """
    index_with(*versions)
    p = run_alr("show", "crate", quiet=False)
    assert_not_substring("Skipping invalid index manifest", p.out)
    run_alr("index", "--check")


# Build metadata does not distinguish versions
check_duplicates("1.0.0", "1.0.0+x")
check_duplicates("1.0.0+a", "1.0.0+b")

# A version with leading zeros is invalid by itself, so it cannot shadow the
# release it would otherwise duplicate
index_with("01.0.0", "1.0.0")
assert_invalid_manifest("leading zeros", "show", "crate", command_fails=False)
p = run_alr("show", "crate", quiet=False)
assert_match(".*crate=1.0.0", p.out)

# Different versions, with or without build metadata or pre-releases, are fine
check_no_duplicates("1.0.0", "1.0.1+x")
check_no_duplicates("1.0.0-rc1", "1.0.0")
check_no_duplicates("1.0.0+x")

print("SUCCESS")
