"""
The version in a manifest file name and in its contents must be the same,
including build metadata (which version equality otherwise ignores), so the
file name cannot misrepresent the release. Short forms of the same version
are fine. Versions that do not parse are reported as such.
"""

import re

from drivers.alr import run_alr, use_single_manifest_index
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


def check(file_version, internal_version, valid, error="Mismatched versions"):
    use_single_manifest_index(MANIFEST.format(version=internal_version),
                              f"crate-{file_version}.toml")
    if valid:
        p = run_alr("show", "crate", quiet=False)
        assert_not_substring("Skipping invalid index manifest", p.out)
        assert_match(".*crate=" + re.escape(file_version), p.out)
        run_alr("index", "--check")
    else:
        assert_invalid_manifest(error, "show", "crate")


# Different build metadata, or pre-release, are rejected
check("1.0.0+a", "1.0.0+b", valid=False)
check("1.0.0+a", "1.0.0", valid=False)
check("1.0.0", "1.0.0+a", valid=False)
check("1.0.0-rc1", "1.0.0-rc2", valid=False)

# Versions that do not parse, in either place, are regular errors, not
# unexpected ones

check("2025.04.18", "2025.4.18", valid=False,
      error="invalid version 2025.04.18 in file name: .*leading zeros")
check("2025.4.18", "2025.04.18", valid=False,
      error="invalid version 2025.04.18 in contents: .*leading zeros")
check("1.0.0", "1.0.0-a..b", valid=False,
      error="invalid version 1.0.0-a..b in contents: ")

# Short forms of the same version are accepted
check("1.6.0", "1.6", valid=True)
check("20240224.0.0", "20240224", valid=True)
check("1.0.0+a", "1.0+a", valid=True)

print("SUCCESS")
