"""
Check transitive links (a pinned dir that contains pinned dirs itself)
"""

import os
import re

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq

# The test will create ./indirect, ./direct, and ./nest/base crates.
# Then they are pinned as base -> direct -> indirect. As "base" has a different
# relative path to "indirect" than "direct", this is also checked.

init_local_crate(name="indirect", binary=False, enter=False)

# Now create "direct" and pin to "indirect"
init_local_crate(name="direct", binary=False)
run_alr("with", "--use=../indirect")

# Now create "base" and pin to "direct"
os.chdir("..")
os.mkdir("nest")
os.chdir("nest")
init_local_crate(name="base")
run_alr("with", "--use=../../direct")

# Verify created pins
p = run_alr("pin")
assert_eq("direct   file:../../direct\n"
          "indirect file:../../indirect\n",
          p.out)

# Check pin removal
os.chdir("../../direct")
run_alr("with", "--del", "indirect")
os.chdir("../nest/base")
run_alr("update")

p = run_alr("pin")
assert_eq("direct file:../../direct\n",
          p.out)


print('SUCCESS')
