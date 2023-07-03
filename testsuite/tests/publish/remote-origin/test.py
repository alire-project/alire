"""
Tests for proper publishing of a ready remote origin
"""

from drivers.alr import run_alr, index_version
from drivers.asserts import assert_match
from drivers.helpers import contents, content_of, init_git_repo, zip_dir
from shutil import copyfile, rmtree
from zipfile import ZipFile

import os


def verify_manifest():
    target = os.path.join("alire", "releases", "xxx-0.1.0-dev.toml")
    assert os.path.isfile(target), \
        "Index manifest not found at expected location"
    # Minimally check the expected contents are in the file
    assert "[origin]" in content_of(target)
    assert "url" in content_of(target)

# create and add an index to check the manifest is loadable later on
os.makedirs(os.path.join("my_index", "xx", "xxx"))
with open(os.path.join("my_index", "index.toml"), "wt") as index_metadata:
    index_metadata.write(f"version = '{index_version()}'\n")
run_alr("index", "--add", "my_index", "--name", "my_index")

# Prepare a repo and a zipball to be used as "remote" targets for publishing
run_alr("init", "--bin", "xxx")

# Create the zip
zip_dir("xxx", "xxx.zip")

# Create repo with the sources
head_commit = init_git_repo("xxx")

# A "remote" source archive. We force to allow the test to skip the remote
# check. Curl requires an absolute path to work.
target = os.path.join(os.getcwd(), "xxx.zip")
run_alr("publish", f"file:{target}", "--skip-submit", force=True)
# Should complete without error, check the generated file is in place
verify_manifest()

# Clean up for the next test
rmtree("alire")

# Same test, using directly the source repository
target = os.path.join(os.getcwd(), "xxx")
run_alr("publish", f"git+file:{target}", head_commit, "--skip-submit", force=True)
verify_manifest()

# Copy the new index manifest into the index
copyfile(os.path.join("alire", "releases", "xxx-0.1.0-dev.toml"),
         os.path.join("my_index", "xx", "xxx", "xxx-0.1.0-dev.toml"))

p = run_alr("search", "--crates")
assert "xxx" in p.out, "Crate not found in index contents"

print('SUCCESS')
