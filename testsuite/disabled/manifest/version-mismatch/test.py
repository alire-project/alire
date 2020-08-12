"""
Check that a local manifest with wrong metadata version is indeed reported
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from os import chdir

import re

# Create a regular working release
run_alr("init", "--bin", "xxx")
chdir("xxx")

# Change the version
with open("alire/xxx.toml", "r") as file:
    lines = file.readlines()
with open("alire/xxx.toml", "w") as file:
    for line in lines:
        if line.startswith("metadata-version"):
            file.write("metadata-version = '0.0'\n")
        else:
            file.write(line)

# Verify the error
p = run_alr('show', complain_on_error=False)
assert p.status != 0, "command should have errored"
assert_match(".*" +
             "Mismatch between manifest version" +
             " \(.*\) and alr index version \(.*\)" +
             ".*",
             p.out, flags=re.S)

print("SUCCESS")
