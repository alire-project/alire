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
             re.escape("""   Pre_Build run: ${CRATE_DIR}/./echo 1
   case OS is
      when others => Pre_Build run: ${CRATE_DIR}/./echo 2
   case OS is
      when others => Pre_Build run: ${CRATE_DIR}/./echo 3
   Pre_Build run: ${CRATE_DIR}/./echo 4
   case OS is
      when others => Pre_Build run: ${CRATE_DIR}/./echo 5
""") + ".*",
             run_alr("show").out)

# Verify actions run in the proper order
assert_eq("1\n2\n3\n4\n5\n",
          run_alr("action", "pre-build").out)

print('SUCCESS')
