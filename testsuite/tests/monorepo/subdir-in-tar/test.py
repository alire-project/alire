"""
Test that "subdir" is rejected for source archive origins
"""

from drivers.alr import init_local_crate, run_alr, alr_copy_to_index
from glob import glob
from shutil import copyfile
from subprocess import run

import drivers.helpers
import os

# Prepare our "remote" repo
init_local_crate("xxx", enter=True)

# Publish it. We need to give input to alr, so we directly call it. We use the
# generated location as the "online" location, and this works because we are
# forcing.
p = run(["alr", "-q", "-f", "-n", "publish", "--skip-build", "--skip-submit", "--tar"],
        input=f"file:{os.getcwd()}/alire/archives/xxx-0.1.0-dev.tbz2\n".encode())
p.check_returncode()

# Add improper subdir to manifest
with open("alire/releases/xxx-0.1.0-dev.toml", "at") as file:
    file.write("subdir='.'\n")

# Submit manifest to index
os.chdir("..")
alr_copy_to_index("xxx/alire/releases/xxx-0.1.0-dev.toml", "my_index")

# Should complain on subdir field
p = run_alr("show", "xxx", complain_on_error=False)
assert "forbidden extra entries: subdir" in p.out, \
    "Unexpected output: " + p.out

print('SUCCESS')
