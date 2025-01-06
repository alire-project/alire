"""
Test the concise mode of the --tree switch. Repeating dependencies are
substituted by a "..." ellipsis.
"""

import os
import re
from drivers.alr import run_alr, alr_with, init_local_crate
from drivers.asserts import assert_eq, assert_match

# Prepare a crate in which dependencies appear twice, so their dependencies in
# turn can be elided.

init_local_crate("yyy")
alr_with("hello")
os.chdir("..")
init_local_crate("xxx")
alr_with("hello")
alr_with("yyy", path="../yyy")

# Check the concise tree
assert_eq("""\
xxx=0.1.0-dev
+-- hello=1.0.1 (*)
|   +-- libhello=1.0.0 (^1.0)
+-- yyy=0.1.0-dev (*)
    +-- hello=1.0.1 (*) ...\
""",
           run_alr("with", "--tree").out.strip())

# Check the regular tree
assert_match(".*" + re.escape("""\
xxx=0.1.0-dev
+-- hello=1.0.1 (*)
|   +-- libhello=1.0.0 (^1.0)
+-- yyy=0.1.0-dev (*)
    +-- hello=1.0.1 (*)
        +-- libhello=1.0.0 (^1.0)\
"""),
           run_alr("-v", "with", "--tree", quiet=False).out.strip())

print("SUCCESS")
