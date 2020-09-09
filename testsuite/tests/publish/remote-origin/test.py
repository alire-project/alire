"""
Tests for proper publishing of a ready remote origin
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import contents, content_of
from shutil import copyfile, rmtree
from subprocess import run
from zipfile import ZipFile

import os


def verify_manifest():
    target = os.path.join("alire", "releases", "xxx-0.0.0.toml")
    assert os.path.isfile(target), \
        "Index manifest not found at expected location"
    # Minimally check the expected contents are in the file
    assert "[origin]" in content_of(target)
    assert "url" in content_of(target)

# create and add an index to check the manifest is loadable later on
os.makedirs(os.path.join("my_index", "xx", "xxx"))
with open(os.path.join("my_index", "index.toml"), "wt") as index_metadata:
    index_metadata.write("version = '0.4'\n")
run_alr("index", "--add", "my_index", "--name", "my_index")

# Prepare a repo and a zipball to be used as "remote" targets for publishing
run_alr("init", "--bin", "xxx")
# Remove the alire cache
rmtree(os.path.join("xxx", "alire"))

# Create the zip
with ZipFile("xxx.zip", 'w') as zip:
    for dir, subdirs, files in os.walk("xxx"):
        for file in files:
            abs_file = os.path.join(dir, file)
            zip.write(abs_file,
                      os.path.join("xxx", os.path.basename(abs_file)))

# Create repo with the sources
os.chdir("xxx")
assert run(["git", "init", "."]).returncode == 0
assert run(["git", "config", "user.email", "alr@testing.com"]).returncode == 0
assert run(["git", "config", "user.name", "Alire Testsuite"]).returncode == 0
assert run(["git", "add", "."]).returncode == 0
assert run(["git", "commit", "-m", "initiailze"]).returncode == 0
head_commit = run(["git", "log", "-n1", "--no-abbrev", "--oneline"],
                  capture_output=True).stdout.split()[0]
os.chdir("..")

# A "remote" source archive. We force to allow the test to skip the remote
# check. Curl requires an absolute path to work.
target = os.path.join(os.getcwd(), "xxx.zip")
run_alr("publish", f"file:{target}", "--force")
# Should complete without error, check the generated file is in place
verify_manifest()

# Clean up for the next test
rmtree("alire")

# Same test, using directly the source repository
target = os.path.join(os.getcwd(), "xxx")
run_alr("publish", f"git+file:{target}", head_commit.decode(), "--force")
verify_manifest()

# Copy the new index manifest into the index
copyfile(os.path.join("alire", "releases", "xxx-0.0.0.toml"),
         os.path.join("my_index", "xx", "xxx", "xxx-0.0.0.toml"))

p = run_alr("list")
assert "xxx" in p.out, "Crate not found in index contents"

print('SUCCESS')
