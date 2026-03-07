"""
Test conditional actions, mixed with unconditional ones
"""

import re

from drivers.alr import run_alr, init_local_crate, alr_manifest
from drivers.asserts import assert_eq, assert_match

init_local_crate()

# Edit the manifest to include all kinds of actions
with open(alr_manifest(), "at") as manifest:
    manifest.write("""
[[actions]]
type = "pre-build"
command = ["echo", "1"]

[[actions.'case(os)'.'...']]
type = "pre-build"
command = ["echo", "2"]

[[actions]]
[actions.'case(os)'.'...']
type = "pre-build"
command = ["echo", "3"]

[[actions]]
type = "pre-build"
command = ["echo", "4"]
    [actions.'case(os)'.'...']
    type = "pre-build"
    command = ["echo", "5"]
""")

# Verify actions are there
assert_match(".*" +
             re.escape("""   Pre_Build run: echo 1 (from ${CRATE_ROOT}/.)
   case OS is
      when others => Pre_Build run: echo 2 (from ${CRATE_ROOT}/.)
   case OS is
      when others => Pre_Build run: echo 3 (from ${CRATE_ROOT}/.)
   Pre_Build run: echo 4 (from ${CRATE_ROOT}/.)
   case OS is
      when others => Pre_Build run: echo 5 (from ${CRATE_ROOT}/.)
""") + ".*",
             run_alr("show").out)

# Verify actions run in the proper order
assert_eq("1\n2\n3\n4\n5\n",
          run_alr("action", "pre-build").out)

print('SUCCESS')
