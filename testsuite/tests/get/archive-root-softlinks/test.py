"""
Softlinks in the top level of a source archive, when moving its contents up
after extraction: all kinds must be moved as links, including dangling ones, and
a link in place of the single expected root folder must be rejected without
touching what it points to.
"""

import hashlib
import os
import shutil
import tarfile

from drivers.alr import crate_dirname, run_alr
from drivers.asserts import assert_eq, assert_substring

# Softlinks directly under the root folder of the archive

run_alr("get", "softlinks")

cratedir = crate_dirname("softlinks")
assert os.path.isfile(os.path.join(cratedir, "x")), \
    f"Missing expected file 'x' in {cratedir}"

# readlink fails if the entry was replaced by what it points to
for link, target in [("to_file", "x"), ("to_dir", "sub"), ("broken", "missing")]:
    assert_eq(target, os.readlink(os.path.join(cratedir, link)))

# The links that are not dangling still lead to their targets
assert os.path.isfile(os.path.join(cratedir, "to_file"))
assert os.path.isfile(os.path.join(cratedir, "to_dir", "y"))

# The testsuite driver's own cleanup fails on dangling links, see the
# get/broken-softlink test
shutil.rmtree(cratedir)

# A softlink to a folder outside of the deployment as the only content of the
# archive, instead of a folder. The target path must be absolute, so the archive
# and its manifest are created here.

OUTSIDE = os.path.abspath("outside")
os.mkdir(OUTSIDE)
PRECIOUS = os.path.join(OUTSIDE, "precious")
with open(PRECIOUS, "w") as f:
    f.write("do not delete\n")

ARCHIVE = os.path.abspath("my_index/rootlink-0.1.0.tgz")
with tarfile.open(ARCHIVE, "w:gz") as tar:
    link = tarfile.TarInfo("rootlink-0.1.0")
    link.type = tarfile.SYMTYPE
    link.linkname = OUTSIDE
    tar.addfile(link)

with open(ARCHIVE, "rb") as f:
    digest = hashlib.sha256(f.read()).hexdigest()

os.makedirs("my_index/index/ro/rootlink")
with open("my_index/index/ro/rootlink/rootlink-0.1.0.toml", "w") as f:
    f.write(f"""\
description = "Example crate"
name = "rootlink"
version = "0.1.0"
licenses = "GPL-3.0-only"
maintainers = ["example@example.com"]
maintainers-logins = ["mylogin"]

[origin]
url = "file:../../../rootlink-0.1.0.tgz"
hashes = ["sha256:{digest}"]
""")

p = run_alr("get", "rootlink", complain_on_error=False)
assert p.status != 0, "Deployment should have failed"
assert os.path.isfile(PRECIOUS), "The target of the softlink was altered"
assert_substring("Unexpected contents where a single directory was expected",
                 p.out)

print('SUCCESS')
