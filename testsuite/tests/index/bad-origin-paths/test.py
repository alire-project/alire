"""
Test that origin paths in index manifests cannot point outside the folder they
are relative to: `subdir` must be a portable path not going above the repository root, and
`archive-name` a plain file name.
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_invalid_manifest

MANIFEST = "my_index/index/he/hello_world/hello_world-0.1.0.toml"

HEADER = """\
description = "\\"Hello, world!\\" demonstration project"
name = "hello_world"
version = "0.1.0"
licenses = "GPL-3.0-only"
maintainers = ["user@example.com"]
maintainers-logins = ["mylogin"]
"""

GIT_ORIGIN = """
[origin]
url = "git+https://github.com/example/hello_world.git"
commit = "0123456789abcdef0123456789abcdef01234567"
subdir = '{}'
"""

ARCHIVE_ORIGIN = """
[origin]
url = "https://example.com/hello_world.tar.gz"
archive-name = '{}'
hashes = ["sha512:deadbeef"]
"""


def write_manifest(origin: str, path: str):
    os.makedirs(os.path.dirname(MANIFEST), exist_ok=True)
    with open(MANIFEST, "w") as f:
        f.write(HEADER + origin.format(path))


# Paths that would escape the deployment folder, or that are not valid on
# every platform, are rejected

ESCAPES = "it must not go above the root of its origin"

for subdir, reason in [("..", ESCAPES),
                       ("../other", ESCAPES),
                       ("./sub/../../other/", ESCAPES),
                       ("/abs/path", "it must be a relative path"),
                       ("C:/abs/path", "it must not contain ':'"),
                       ("sub\\dir", "it must use forward slashes"),
                       ("\\\\server\\share", "it must use forward slashes")]:
    write_manifest(GIT_ORIGIN, subdir)
    assert_invalid_manifest("invalid subdir '" + re.escape(subdir) + "': "
                            + re.escape(reason))

for name, reason in [("../hello_world.tar.gz", "plain file name"),
                     ("/tmp/hello_world.tar.gz", "plain file name"),
                     ("dir/hello_world.tar.gz", "plain file name"),
                     ("..\\hello_world.tar.gz", "plain file name"),
                     ("C:hello_world.tar.gz", "plain file name"),
                     ("..", "it must be a file name")]:
    write_manifest(ARCHIVE_ORIGIN, name)
    assert_invalid_manifest("invalid archive name '" + re.escape(name)
                            + "': .*" + re.escape(reason))

# Valid paths, as used in the community index, still load

for subdir in ["sub", "./sub/", "sub/dir", "./.alire/sub/", "a..b",
               "sub/../other"]:
    write_manifest(GIT_ORIGIN, subdir)
    run_alr("index", "--check")
    run_alr("show", "hello_world")

write_manifest(ARCHIVE_ORIGIN, "hello_world-0.1.0.tar.gz")
run_alr("index", "--check")
run_alr("show", "hello_world")

print("SUCCESS")
