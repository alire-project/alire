"""
Verify that catalog.autoconfig works as intended (no autoadd community index)
"""

from glob import glob
import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match


# Since no index is configured, looking for crates will attempt to add the
# community index
p = run_alr("search", "hello", quiet=False)
assert \
    "Not configuring the community index, " + \
    "disabled via index.auto_community" \
    in p.out, "unexpected output: " + p.out


print('SUCCESS')
