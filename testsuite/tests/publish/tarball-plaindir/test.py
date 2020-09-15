"""
Tests tarball publishing from non-vcs directory
"""

from drivers.alr import init_local_crate, run_alr
from shutil import copyfile
from subprocess import run

import os

# Prepare our "remote" repo
init_local_crate("xxx", enter=True)

# Publish it. We need to give input to alr, so we directly call it. We use the
# generated location as the "online" location, and this works because we are
# forcing.
p = run(["alr", "publish", "--skip-build", "--tar", "-q", "-f", "-n"],
        input=f"file:{os.getcwd()}/alire/archives/xxx-0.0.0.tbz2\n".encode())
p.check_returncode()

# Verify the index manifest has been generated
assert os.path.isfile("./alire/releases/xxx-0.0.0.toml")

os.chdir("..")

# Add this manifest to our local index, and retrieve + build the crate
os.makedirs("my_index/index/xx/xxx")
copyfile("xxx/alire/releases/xxx-0.0.0.toml",
         "my_index/index/xx/xxx/xxx-0.0.0.toml")

run_alr("get", "--build", "xxx")  # Should not err

print('SUCCESS')
