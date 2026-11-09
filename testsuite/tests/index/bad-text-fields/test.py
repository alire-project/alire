"""
Test that text fields in index manifests cannot carry control characters
(which could form terminal escape sequences when shown) and are bounded in
length, while ordinary multi-line and Unicode texts still load.
"""

import json
import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_invalid_manifest

MANIFEST = "my_index/index/he/hello_world/hello_world-0.1.0.toml"

HEADER = """\
name = "hello_world"
version = "0.1.0"
licenses = "GPL-3.0-only"
maintainers = ["user@example.com"]
"""

ORIGIN = """
[origin]
url = "https://example.com/hello_world.tar.gz"
hashes = ["sha512:deadbeef"]
"""


def write_manifest(fields: dict[str, str]):
    """Write the manifest with the given string fields (json.dumps gives
    valid TOML basic strings, with control characters escaped)"""
    fields = {"description": "d", "maintainers-logins": "mylogin"} | fields
    os.makedirs(os.path.dirname(MANIFEST), exist_ok=True)
    with open(MANIFEST, "w", encoding="utf-8") as f:
        f.write(HEADER)
        for key, value in fields.items():
            text = json.dumps(value, ensure_ascii=False)
            f.write(f"{key} = [{text}]\n" if key in ["maintainers-logins",
                                                     "authors"]
                    else f"{key} = {text}\n")
        f.write(ORIGIN)


CONTROL = "control character at position"

for key, value, error in [
        # Terminal escape sequences, in single- and multi-line fields
        ("description", "\x1b[31mred", f"description: {CONTROL} 1"),
        ("long-description", "line\x1b]0;title\x07", CONTROL),
        ("notes", "a\u009b31m", f"notes: {CONTROL} 2"),  # C1 CSI
        ("authors", "Bell\x07", CONTROL),
        ("maintainers-logins", "login\x1b", CONTROL),
        # Carriage returns can overwrite what was shown before them
        ("long-description", "shown\rhidden", CONTROL),
        # Line breaks and tabs only in multi-line fields
        ("description", "two\nlines", CONTROL),
        ("website", "https://example.com/\t", CONTROL),
        # Lengths
        ("website", "https://example.com/" + "x" * 493,
         "website is too long: 513 characters, the maximum is 512"),
        ("long-description", "x" * 16385,
         "long-description is too long: 16385 characters,"
         " the maximum is 16384"),
        ("notes", "x" * 2049,
         "notes is too long: 2049 characters, the maximum is 2048")]:
    write_manifest({key: value})
    assert_invalid_manifest(re.escape(error))

# Lengths count characters, not bytes

write_manifest({"description": "\u00e9" * 72})
run_alr("index", "--check")

# Valid texts still load

write_manifest({"description": "Café € 中",
                "long-description": "Para 1\n\n\tIndented\n" + "x" * 16000,
                "notes": "Line 1\nLine 2",
                "website": "https://example.com/" + "x" * 492})
run_alr("index", "--check")
run_alr("show", "hello_world")

print("SUCCESS")
