"""
Check that a locally broken manifest is reported with its full error
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match
from drivers.helpers import contents, content_of, init_git_repo, zip_dir
from shutil import copyfile, rmtree
from zipfile import ZipFile

import os


# Prepare a repo for publishing and break its manifest
init_local_crate()
# Break the manifest
with open("alire.toml", "wt") as manifest:
    manifest.write("\n---\n")

# Attempt to publish; should fail with the expected syntax error
p = run_alr("publish", "--force", complain_on_error=False)
assert_match(".*invalid syntax at.*alire\.toml.*", p.out)

print('SUCCESS')
