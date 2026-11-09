"""
System package names reach package-manager commands, so names that a package
manager would not take as a repository package are rejected when loading the
index: options, local paths, URLs, patterns, whitespace. Other names, even
unusual ones, are accepted.
"""

import re

from drivers.alr import run_alr, use_single_manifest_index
from drivers.asserts import (assert_invalid_manifest, assert_match,
                             assert_not_substring)

EXTERNAL = """\
name = "ext"
description = "External with system packages"
maintainers = ["example@example.com"]
maintainers-logins = ["mylogin"]

[[external]]
kind = "system"
{origin}
"""

RELEASE = """\
name = "sys_url"
version = "1.0.0"
description = "Release with a system origin"
maintainers = ["example@example.com"]
maintainers-logins = ["mylogin"]

[origin]
url = "system:{name}"
"""


def check_rejected(name, reason, origin=None):
    """
    Check that an external with `name` among its packages is rejected
    """
    use_single_manifest_index(
        EXTERNAL.format(origin=origin or f"origin = [{name!r}]"),
        "ext-external.toml")
    assert_invalid_manifest(
        f"invalid system package name '{re.escape(name)}': {reason}",
        "show", "ext")


FIRST_CHAR = "it must begin with a letter or digit"

# Options, which apt-get would run as root (e.g. DPkg::Pre-Invoke)
check_rejected("-oDPkg::Pre-Invoke::=touch pwned", FIRST_CHAR)
check_rejected("+variant", FIRST_CHAR)

# Local paths, apt patterns, emerge sets
for name in ["/tmp/evil.deb", "./evil.deb", "~nfoo", "?name(foo)", "@world"]:
    check_rejected(name, FIRST_CHAR)

# URLs, which begin with a letter
check_rejected("https://example.com/evil.rpm", "it must not be a URL")

# Whitespace, which no package manager uses
check_rejected("lib foo", "it must not contain whitespace or control "
                          "characters")

# A bad name only in the branch of another distribution is still rejected,
# whatever the current platform
check_rejected("--help", FIRST_CHAR,
               origin='[external.origin."case(distribution)"]\n'
                      'debian = ["ok", "--help"]\n'
                      'ubuntu = ["ok"]')

# The same rule applies to `system:` origins in releases
use_single_manifest_index(RELEASE.format(name="-oFoo"), "sys_url-1.0.0.toml")
assert_invalid_manifest(
    "invalid system package name '-oFoo': " + FIRST_CHAR, "show", "sys_url")

# Unusual but legal names, from real indexes and package managers, load fine
use_single_manifest_index(
    EXTERNAL.format(origin='origin = ["dev-libs/gmp", "gtk+3", "openssl@3", '
                           '"libglib2.0-dev", "libfoo:amd64", '
                           '"SDL2_image-devel", "pkg[feature]", '
                           '"libhello~1"]'),
    "ext-external.toml")
p = run_alr("show", "ext", "--external", quiet=False)
assert_not_substring("Skipping invalid index manifest", p.out)
assert_match(".*dev-libs/gmp.*pkg\\[feature\\].*libhello~1", p.out)
run_alr("index", "--check")

print("SUCCESS")
