"""
Test that manifests larger than the limit in `index.max_manifest_size` are
skipped (or rejected by `alr index --check`) before being parsed.
"""

import os

from drivers.alr import alr_settings_set, run_alr
from drivers.asserts import assert_invalid_manifest

MANIFEST = "my_index/index/he/hello_world/hello_world-0.1.0.toml"

CONTENTS = """\
description = "\\"Hello, world!\\" demonstration project"
name = "hello_world"
version = "0.1.0"
licenses = "GPL-3.0-only"
maintainers = ["user@example.com"]
maintainers-logins = ["mylogin"]

[origin]
url = "https://example.com/hello_world.tar.gz"
hashes = ["sha512:deadbeef"]
"""


def write_manifest(size: int):
    """Write the manifest, padded with a comment to the given size"""
    os.makedirs(os.path.dirname(MANIFEST), exist_ok=True)
    padding = size - len(CONTENTS) - len("#\n")
    with open(MANIFEST, "w", newline="\n") as f:
        f.write(CONTENTS + "#" + "x" * padding + "\n")
    assert os.path.getsize(MANIFEST) == size


TOO_LARGE = "manifest is larger than 65536 bytes" \
            " \\(see setting index.max_manifest_size\\)"

# The default limit is inclusive

write_manifest(65536)
run_alr("index", "--check")
run_alr("show", "hello_world")

write_manifest(65537)
assert_invalid_manifest(TOO_LARGE)

# The limit can be raised, or lowered, through settings

alr_settings_set("index.max_manifest_size", "100000")
run_alr("index", "--check")
run_alr("show", "hello_world")

alr_settings_set("index.max_manifest_size", "1000")
write_manifest(1001)
assert_invalid_manifest("manifest is larger than 1000 bytes")

print("SUCCESS")
