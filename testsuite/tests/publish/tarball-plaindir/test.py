"""
Tests tarball publishing from non-vcs directory
"""

from drivers.alr import init_local_crate, run_alr
from glob import glob
from shutil import copyfile
from subprocess import run

import drivers.helpers
import os

# Prepare our "remote" repo
init_local_crate("xxx", enter=True)

canary = "canary.txt"

# Create a canary file to double-check that it does not make into the tarball
with open(os.path.join("alire", canary), "wt") as file:
    print(file, "...\n")

# Publish it. We need to give input to alr, so we directly call it. We use the
# generated location as the "online" location, and this works because we are
# forcing.
p = run(["alr", "-q", "-f", "-n", "publish", "--skip-build", "--skip-submit", "--tar"],
        input=f"file:{os.getcwd()}/alire/archives/xxx-0.1.0-dev.tbz2\n".encode())
p.check_returncode()

# Verify the generated file does not contain the alire folder
p = run(["tar", "tf", "alire/archives/xxx-0.1.0-dev.tbz2"],
        capture_output=True)
p.check_returncode()
assert "xxx-0.0.0/alire/" not in p.stdout.decode(), \
    "Unexpected contents in tarball: " + p.stdout.decode()

# Verify the index manifest has been generated
assert os.path.isfile("./alire/releases/xxx-0.1.0-dev.toml")

os.chdir("..")

# Add this manifest to our local index, and retrieve + build the crate
os.makedirs("my_index/index/xx/xxx")
copyfile("xxx/alire/releases/xxx-0.1.0-dev.toml",
         "my_index/index/xx/xxx/xxx-0.1.0-dev.toml")

run_alr("get", "--build", "xxx")  # Should not err

# Verify the canary didn't made through
os.chdir(glob("xxx_*")[0])
assert not os.path.isfile(os.path.join("alire", canary)), \
    "Found canary file that should not be there"

print('SUCCESS')
