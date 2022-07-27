"""
Test the output of `alr show` for an abstract crate
"""

import subprocess
import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq

# gnat does not contain releases but several other crates provide it

assert_eq("""Crate gnat is abstract and provided by:
   gnat_cross_1
   gnat_cross_2
   gnat_external
   gnat_native
""",
          run_alr("show", "gnat", quiet=False).out)

print('SUCCESS')
