"""
Verify that printenv output is correct for regular and pinned crates when
invoked not from the root of the workspace.
"""

import os
import re
from drivers.alr import alr_pin, init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_match

parent = os.getcwd()

init_local_crate("base")
init_local_crate("pinned", enter=False)
alr_pin("pinned", path="pinned")

os.chdir("src") # Enter a subfolder
p = run_alr("printenv")

# Verify root crate proper path in GPR_PROJECT_PATH
assert_match(r".*GPR_PROJECT_PATH[^\n]+"
             + re.escape(os.path.join(parent, "base"))
             + "(:|;|\")", p.out)

# Verify pinned crate proper path in GPR_PROJECT_PATH
assert_match(r".*GPR_PROJECT_PATH[^\n]+"
             + re.escape(os.path.join(parent, "base", "pinned"))
             + "(:|;|\")", p.out)

print("SUCCESS")
