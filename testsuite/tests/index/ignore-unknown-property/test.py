"""
Check that unknown properties can be force-ignored
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from os import chdir

import re

# Create a regular working release
run_alr("init", "--bin", "xxx")
chdir("xxx")

# Add spurious metadata
with open("alire/xxx.toml", "r") as file:
    lines = file.readlines()
with open("alire/xxx.toml", "w") as file:
    file.write("fancy-new-feat = false\n")
    file.writelines(lines)

# Verify the regular error
p = run_alr('show', complain_on_error=False)
assert p.status != 0, "command should have errored"
assert_match(".*invalid property:.*", p.out, flags=re.S)

# Verify the force-ignore
p = run_alr('show', quiet=False, force=True)     # Should not complain
assert_match(".*Warning:.*invalid property:.*",  # Should be a warning
             p.out, flags=re.S)

print("SUCCESS")
