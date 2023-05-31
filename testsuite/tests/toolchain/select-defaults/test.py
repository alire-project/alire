"""
Check toolchain autoconfiguration on 1st run without assistant
"""

import re

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match

init_local_crate()

# Re-config as if it was the first time running alr
run_alr("config", "--global", "--set", "toolchain.assistant", "true")

# A command requiring a workspace will trigger the assistant
p = run_alr("show", quiet=False)

# Verify that the newest tools have been selected
assert_match(".*" + re.escape(
"""\
Alire has selected automatically this toolchain:
   gprbuild=8888.0.0
   gnat_native=8888.0.0"""),
   p.out)

# Verify that the internal config matches what was said
p = run_alr("version")
assert_match(".*" + re.escape(
"""\
tool #1 gnat:              gnat_native=8888.0.0
tool #2 gprbuild:          gprbuild=8888.0.0"""),
   p.out)

print('SUCCESS')
