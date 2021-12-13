"""
Check detection of a remote without manifest. This was allowed in the past, so
legacy repositories that ignored the `alire.toml` file may run amok of this
test.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import contents, content_of, init_git_repo, zip_dir
from shutil import copyfile, rmtree
from zipfile import ZipFile

import os


# Prepare a repo and a zipball to be used as "remote", without a manifest
run_alr("init", "--bin", "xxx")
# Remove the manifest
os.remove(os.path.join("xxx", "alire.toml"))

# Create the zip
zip_dir("xxx", "xxx.zip")

# A "remote" source archive. We force to allow the test to skip the remote
# check. Curl requires an absolute path to work.
target = os.path.join(os.getcwd(), "xxx.zip")
p = run_alr("publish", f"file:{target}", "--skip-build",
            complain_on_error=False, force=True)

# Should fail reporting the missing manifest
assert_match(".*Remote sources are missing the 'alire.toml' manifest file.*",
             p.out)

print('SUCCESS')
