"""
Check that a local manifest without version is reported as old
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from os import chdir

import re

# Create a regular working release
run_alr("init", "--bin", "xxx")
chdir("xxx")

# Remove the version
with open("alire/xxx.toml", "r") as file:
    lines = file.readlines()
with open("alire/xxx.toml", "w") as file:
    for line in lines:
        if not line.startswith("metadata-version"):
            file.write(line)

# Verify the error
p = run_alr('show', complain_on_error=False)
assert p.status != 0, "command should have errored"
assert_match(".*" +
             "Local manifest file is for an old alr version" +
             re.escape(" (lacks metadata-version field)") +
             ".*",
             p.out, flags=re.S)

print("SUCCESS")
