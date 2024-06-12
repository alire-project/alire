"""
Tests tarball publishing from a git repository
"""

from drivers.alr import init_local_crate, run_alr
from drivers.helpers import init_git_repo
from shutil import copyfile
from subprocess import run

import os

# Prepare our "remote" repo
init_local_crate("xxx", enter=True)

# Initialize a repo right here
init_git_repo(".")

# Clone it to simulate it's our local copy
os.chdir("..")
os.rename("xxx", "xxx_upstream")
run(["git", "clone", "xxx_upstream", "xxx"]).check_returncode()
os.chdir("xxx")

# Publish it. We need to give input to alr, so we directly call it. We use the
# generated location as the "online" location, and this works because we are
# forcing. ".tgz" is used, as bzip2 is not supported by `git archive`.
p = run(["alr", "-q", "-f", "-n", "publish", "--skip-build", "--skip-submit", "--tar"],
        input=f"file:{os.getcwd()}/alire/archives/xxx-0.1.0-dev.tgz\n".encode())
p.check_returncode()

# Verify the index manifest has been generated
assert os.path.isfile("./alire/releases/xxx-0.1.0-dev.toml")

os.chdir("..")

# Add this manifest to our local index, and retrieve + build the crate
os.makedirs("my_index/index/xx/xxx")
copyfile("xxx/alire/releases/xxx-0.1.0-dev.toml",
         "my_index/index/xx/xxx/xxx-0.1.0-dev.toml")

run_alr("get", "--build", "xxx")  # Should not err

print('SUCCESS')
