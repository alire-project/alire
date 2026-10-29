"""
Test that symbolic links in an index are rejected unless the setting
index.allow_symlinks is enabled, for links to manifests and to folders, both
when loading the whole index and a single crate.
"""

import os
import re
import shutil

from drivers.alr import alr_settings_set, run_alr
from drivers.asserts import assert_invalid_manifest, assert_match

MANIFEST = """\
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

INDEX = os.path.abspath("my_index/index")
OUTSIDE = os.path.abspath("outside")  # Where links point to
LINK = "symbolic link in index: "
SETTING = r" \(see setting index.allow_symlinks\)"


def reset():
    """Leave the index with only its metadata, and the real files outside"""
    shelf = os.path.join(INDEX, "he")
    if os.path.islink(shelf):
        os.unlink(shelf)
    shutil.rmtree(shelf, ignore_errors=True)
    shutil.rmtree(OUTSIDE, ignore_errors=True)
    os.makedirs(os.path.join(OUTSIDE, "he", "hello_world"))
    with open(os.path.join(OUTSIDE, "he", "hello_world",
                           "hello_world-0.1.0.toml"), "w") as f:
        f.write(MANIFEST)


def link(target: str, path: str):
    os.makedirs(os.path.dirname(os.path.join(INDEX, path)), exist_ok=True)
    os.symlink(os.path.join(OUTSIDE, target), os.path.join(INDEX, path))


# A link to a manifest, a crate folder or a shelf folder is rejected, both
# in full loads (assert_invalid_manifest) and single-crate loads (second
# `show`, now that the providers cache exists)

for target, path in [
        ("he/hello_world/hello_world-0.1.0.toml",
         "he/hello_world/hello_world-0.1.0.toml"),
        ("he/hello_world", "he/hello_world"),
        ("he", "he")]:
    reset()
    link(target, path)
    assert_invalid_manifest(re.escape(LINK + os.path.join(INDEX, path))
                            + SETTING)
    p = run_alr("show", "hello_world", complain_on_error=False,
                quiet=False)
    assert_match(".*" + re.escape(LINK), p.out)

# When allowed, a link to a manifest is followed, with a warning

alr_settings_set("index.allow_symlinks", "true")
reset()
path = "he/hello_world/hello_world-0.1.0.toml"
link(path, path)
following = ("Warning: Following symbolic link in index: "
             + re.escape(os.path.join(INDEX, path)))
assert_match(".*" + following, run_alr("index", "--check", quiet=False).out)
p = run_alr("show", "hello_world", quiet=False)
assert_match(".*" + following + ".*hello_world=0.1.0", p.out)

# Links to folders are never entered, in full or single-crate loads, also
# with a warning

reset()
link("he/hello_world", "he/hello_world")
for _ in range(2):  # Without and with providers cache
    p = run_alr("show", "hello_world", complain_on_error=False,
                quiet=False)
    assert_match(".*Warning: Not entering symbolic link to folder in index: "
                 + re.escape(os.path.join(INDEX, "he/hello_world"))
                 + ".*hello_world.* does not exist in the index", p.out)

print("SUCCESS")
