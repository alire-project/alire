"""
Publish a crate that contains pins, and verify pins are removed in the process
"""

import os

from drivers.alr import init_local_crate, alr_pin, alr_publish, alr_manifest
from drivers.asserts import assert_match
from drivers.helpers import content_of, init_git_repo, on_windows

from subprocess import run

crate = "pinner"

# We create a repository with the nested crate that will act as the upstream
# remote repository:
start_dir = os.getcwd()
init_local_crate(crate, with_maintainer_login=True)

# And add the pin directly in the remote
alr_pin("unobtanium", path=("c:/" if on_windows() else "/"))

# We can now create the upstream repo
os.chdir("..")
commit = init_git_repo(crate)
os.rename(crate, f"{crate}.upstream")

# We clone the project to obtain our local copy
assert run(["git", "clone", f"{crate}.upstream", crate]).returncode == 0

# We enter the clone
os.chdir(crate)

# Verify the pin is there
assert_match(r".*\[\[pins\]\].*", content_of(alr_manifest()))

# We publish with the pin in the manifest
p = alr_publish(crate, "0.1.0-dev",
                index_path=os.path.join(start_dir, "my_index"),
                copy_to_index=False,
                quiet=False)

# Verify warning during publishing
assert_match(".*contains pins that will be removed.*", p.out)

# Verify no pins in the generated file
assert "[[pins]]" not in \
    content_of(os.path.join("alire", "releases", f"{crate}-0.1.0-dev.toml")), \
    "Unexpected contents in published file"

print('SUCCESS')
